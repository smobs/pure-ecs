-- | ECS Phase 2: World and Archetype Storage
-- |
-- | This module implements the World container with archetype-based component
-- | storage. Archetypes group entities by their component signature, enabling
-- | cache-friendly iteration while phantom row types ensure compile-time safety.
-- |
-- | Key concepts:
-- | - World: Container for all ECS state (entities + archetypes)
-- | - Archetype: Dense storage for entities with same components
-- | - Entity: Phantom-typed wrapper around EntityId
-- | - Type erasure: Foreign for heterogeneous archetype storage
module ECS.World
  ( World
  , Entity
  , Archetype
  , ArchetypeId
  , ComponentMask
  , ComponentRegistry
  , QueryCacheKey
  , CachedQueryResult
  , emptyWorld
  , spawnEntity
  , despawnEntity
  , hasEntity
  , unEntity
  , wrapEntity
  -- Component mask operations
  , getComponentMask
  , getOrCreateComponentMask
  , bitToMask
  , emptyMask
  , mkMask
  , maskContains
  , maskHasAny
  , maskAddBit
  , maskRemoveBit
  -- Query cache helpers
  , makeQueryCacheKey
  , incrementStructuralVersion
  -- Pure versions (for internal use)
  , spawnEntityPure
  , despawnEntityPure
  ) where

import Prelude

import Control.Monad.State (State, runState, state)
import Data.Array (index, length, take, updateAt)
import Data.Array as Array
import Data.Int.Bits (complement, shl, (.&.), (.|.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import ECS.Entity (EntityId, EntityManager, createEntity, deleteEntity, emptyEntityManager, entityIndex, validateEntity)
import ECS.Internal.ComponentStorage (ComponentStorage, arraySwapRemoveAt)
import ECS.Internal.ComponentStorage as CS

-- | Bitmask for fast component matching.
-- |
-- | Each component label is assigned a unique bit position (0, 1, 2, …).
-- | The mask is a packed little-endian array of 32-bit words: word `i` carries
-- | bits `[32*i, 32*i + 32)`. The array grows on demand as new labels register,
-- | so the number of supported components is unbounded.
-- |
-- | INVARIANT (load-bearing): every `ComponentMask` value MUST be canonical —
-- | i.e. have no trailing zero words. The derived `Eq` and `Ord` instances
-- | are positional over the underlying `Array Int`, so two masks representing
-- | the same set of bits compare equal **iff** both are canonical. Breaking
-- | this invariant silently corrupts `world.archetypes` and `world.queryCache`
-- | (they're `Map`-keyed by `ComponentMask`): a non-canonical mask creates a
-- | parallel archetype key invisible to queries built from canonical masks.
-- |
-- | All in-module constructors (`emptyMask`, `maskAddBit`, `maskRemoveBit`,
-- | `bitToMask`) preserve the invariant. The raw `ComponentMask` constructor
-- | is NOT exported (see export list) so external callers cannot bypass it.
-- | Inside this module, prefer `mkMask` over the raw constructor when
-- | building a mask from an arbitrary `Array Int`.
-- |
-- | Archetype matching is O(W) where W = number of words in the required
-- | mask (typically 1-2 for small games, ≤ ⌈N/32⌉ in general).
newtype ComponentMask = ComponentMask (Array Int)

derive newtype instance eqComponentMask :: Eq ComponentMask
derive newtype instance ordComponentMask :: Ord ComponentMask

instance showComponentMask :: Show ComponentMask where
  show (ComponentMask ws) = "ComponentMask " <> show ws

-- | Registry mapping component labels to their bit positions.
-- |
-- | Built lazily as new component types are encountered.
-- | Persists across world modifications.
type ComponentRegistry =
  { labelToBit :: Map String Int
  , nextBit :: Int
  }

-- | Archetype ID is the bitmask of component bits.
-- |
-- | Two archetypes are the same iff they have the same set of components,
-- | which the bitmask captures exactly. Keys in `world.archetypes` are masks.
-- | This eliminates per-add/remove string parsing/sorting/joining.
type ArchetypeId = ComponentMask

-- | Key for query cache lookup.
-- |
-- | (requiredMask, excludedMask) — uniquely identifies a query by its
-- | required/excluded component masks. Tuple keys avoid the per-call
-- | string allocation of a formatted key.
type QueryCacheKey = Tuple ComponentMask ComponentMask

-- | Cached query result with version tracking.
-- |
-- | Contains matching archetype IDs computed at a specific structural version.
-- | When structuralVersion changes, cache entries become stale.
type CachedQueryResult =
  { matchingArchetypes :: Array ArchetypeId
  , version :: Int
  }

-- | World contains all ECS state.
-- |
-- | Structure:
-- | - entities: EntityManager for ID lifecycle (Phase 1)
-- | - archetypes: Type-erased archetype storage (Map ArchetypeId Foreign)
-- | - entityLocations: Tracks which archetype contains each entity
-- | - componentRegistry: Maps component labels to bit positions for fast matching
-- | - structuralVersion: Incremented on archetype changes (for cache invalidation)
-- | - queryCache: Cached query results (archetype matching)
type World =
  { entities :: EntityManager
  , archetypes :: Map ArchetypeId Archetype
  , entityLocations :: Map Int ArchetypeId
  , componentRegistry :: ComponentRegistry
  , structuralVersion :: Int
  , queryCache :: Map QueryCacheKey CachedQueryResult
  }

-- | Entity handle with phantom row type.
-- |
-- | The row type exists only at compile-time to track which components
-- | this entity has. At runtime, it's just an EntityId.
-- |
-- | Example types:
-- | - Entity () - empty entity (no components)
-- | - Entity (position :: Position) - has position component
-- | - Entity (position :: Position, velocity :: Velocity) - has both
newtype Entity (components :: Row Type) = Entity EntityId

derive newtype instance eqEntity :: Eq (Entity components)
derive newtype instance ordEntity :: Ord (Entity components)

-- | Archetype storage structure (concrete, no phantom types).
-- |
-- | Contains:
-- | - entities: Array of EntityIds in this archetype
-- | - entityPositions: Map from entity index to position in entities array (O(log N) lookup)
-- | - mask: Bitmask of component types for O(1) query matching
-- | - labels: Cached set of component labels (avoids reparsing archetype ID)
-- | - storage: Component label -> Array Foreign (component values)
-- |
-- | Type safety is enforced at the Entity level via phantom row types,
-- | not at the archetype storage level. All archetypes have the same type.
type Archetype =
  { entities :: Array EntityId
  , entityPositions :: Map Int Int  -- entityIndex -> position in entities array
  , mask :: ComponentMask           -- Bitmask for fast query matching
  , labels :: Set String            -- Cached component labels
  , storage :: ComponentStorage
  }

-- | Empty archetype ID for newly spawned entities (no components).
emptyArchetypeId :: ArchetypeId
emptyArchetypeId = emptyMask

-- | The canonical empty mask. Equal to `bitToMask`-free state.
emptyMask :: ComponentMask
emptyMask = ComponentMask []

-- | Safe smart constructor that canonicalises an arbitrary `Array Int` into
-- | a valid `ComponentMask` by trimming trailing zero words. Use this from
-- | any code that builds a mask from raw word data (deserialisation, FFI).
-- | All other in-module construction paths preserve the canonical-form
-- | invariant by construction and don't need to call this.
mkMask :: Array Int -> ComponentMask
mkMask ws = ComponentMask (trimTrailingZeros ws)

-- | Create an empty world.
-- |
-- | Initial state:
-- | - No entities
-- | - No archetypes (will create "" archetype on first spawn)
-- | - Empty entity location tracking
-- | - Empty component registry
-- | - Structural version 0
-- | - Empty query cache
emptyWorld :: World
emptyWorld =
  { entities: emptyEntityManager
  , archetypes: Map.empty
  , entityLocations: Map.empty
  , componentRegistry: { labelToBit: Map.empty, nextBit: 0 }
  , structuralVersion: 0
  , queryCache: Map.empty
  }

-- | Spawn a new entity (monadic version).
-- |
-- | This is the main API. It works within the State monad over World,
-- | automatically managing world state threading.
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   e' <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |   pure e'
-- | ```
spawnEntity :: State World (Entity ())
spawnEntity = state \world ->
  let result = spawnEntityPure world
  in Tuple result.entity result.world

-- | Spawn a new entity (pure version for internal use).
-- |
-- | Algorithm:
-- | 1. Generate EntityId via EntityManager
-- | 2. Find or create empty archetype ("")
-- | 3. Add EntityId to empty archetype
-- | 4. Update entityLocations map
-- | 5. Return Entity () with empty row type
-- |
-- | Returns both updated World and the new Entity handle.
spawnEntityPure :: World -> { world :: World, entity :: Entity () }
spawnEntityPure world =
  let
    -- Step 1: Create EntityId (see CLAUDE.md State monad pattern)
    (Tuple entityId state) = runState createEntity world.entities

    -- Step 2: Get or create empty archetype
    emptyArch = getOrCreateEmptyArchetype world.archetypes

    -- Step 3: Add entity to empty archetype with position tracking
    newPosition = length emptyArch.entities
    updatedArch = emptyArch
      { entities = emptyArch.entities <> [entityId]
      , entityPositions = Map.insert (entityIndex entityId) newPosition emptyArch.entityPositions
      }
    updatedArchetypes = Map.insert emptyArchetypeId updatedArch world.archetypes

    -- Step 4: Update entity locations
    updatedLocations = Map.insert (entityIndex entityId) emptyArchetypeId world.entityLocations

    -- Step 5: Build updated world
    newWorld = world
      { entities = state
      , archetypes = updatedArchetypes
      , entityLocations = updatedLocations
      }
  in
    { world: newWorld, entity: Entity entityId }

-- | Despawn an entity (monadic version).
-- |
-- | This is the main API. It works within the State monad over World.
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   despawnEntity e  -- Remove it
-- | ```
despawnEntity :: forall r. Entity r -> State World Unit
despawnEntity entity = state \world ->
  Tuple unit (despawnEntityPure entity world)

-- | Despawn an entity (pure version for internal use).
-- |
-- | Algorithm:
-- | 1. Unwrap Entity to get EntityId
-- | 2. Validate entity exists
-- | 3. Find entity's archetype via entityLocations
-- | 4. Remove entity from archetype (swap-remove with position map update)
-- | 5. Delete from entityLocations
-- | 6. Delete EntityId via EntityManager
-- | 7. Return updated World
despawnEntityPure :: forall r. Entity r -> World -> World
despawnEntityPure (Entity entityId) world =
  -- Validate entity exists
  if not (validateEntity entityId world.entities) then
    world  -- Entity doesn't exist, return unchanged
  else
    let
      idx = entityIndex entityId

      -- Find archetype
      maybeArchId = Map.lookup idx world.entityLocations
    in
      case maybeArchId of
        Nothing -> world  -- Entity not in any archetype, return unchanged

        Just archId ->
          let
            -- Get archetype (type-erased)
            maybeArch = Map.lookup archId world.archetypes
          in
            case maybeArch of
              Nothing -> world  -- Archetype missing, return unchanged

              Just arch ->
                let
                  -- Find entity's position in archetype using O(log N) map lookup
                  maybePos = Map.lookup idx arch.entityPositions

                  -- Swap-remove entity from archetype and remove its components
                  updatedArch = case maybePos of
                    Nothing -> arch  -- Entity not found, leave archetype unchanged
                    Just pos ->
                      let
                        lastIdx = length arch.entities - 1

                        -- Swap-remove from entities array and update position map
                        result = if pos == lastIdx then
                          -- Target is last element, just remove
                          { entities: take lastIdx arch.entities
                          , positions: Map.delete idx arch.entityPositions
                          }
                        else
                          -- Swap with last, update swapped entity's position
                          case index arch.entities lastIdx of
                            Nothing ->
                              -- Shouldn't happen, but fallback
                              { entities: arch.entities
                              , positions: arch.entityPositions
                              }
                            Just lastEntity ->
                              let
                                swappedEntities = fromMaybe arch.entities $
                                  updateAt pos lastEntity arch.entities
                                truncatedEntities = take lastIdx swappedEntities
                                -- Update positions: remove target, update swapped entity's position
                                updatedPositions = Map.insert (entityIndex lastEntity) pos $
                                  Map.delete idx arch.entityPositions
                              in
                                { entities: truncatedEntities
                                , positions: updatedPositions
                                }

                        -- Swap-remove from component arrays
                        updatedStorage = CS.mapWithKey (\_ arr ->
                          arraySwapRemoveAt pos arr
                        ) arch.storage
                      in
                        { entities: result.entities
                        , entityPositions: result.positions
                        , mask: arch.mask    -- Mask stays the same
                        , labels: arch.labels  -- Labels stay the same
                        , storage: updatedStorage
                        }

                  updatedArchetypes = Map.insert archId updatedArch world.archetypes

                  -- Remove from entity locations
                  updatedLocations = Map.delete idx world.entityLocations

                  -- Delete EntityId (increments version, adds to free list)
                  (Tuple _ newEntityManager) = runState (deleteEntity entityId) world.entities
                in
                  world
                    { entities = newEntityManager
                    , archetypes = updatedArchetypes
                    , entityLocations = updatedLocations
                    }

-- | Check if entity exists in world.
-- |
-- | Delegates to EntityManager validation (version check).
hasEntity :: forall r. Entity r -> World -> Boolean
hasEntity (Entity entityId) world =
  validateEntity entityId world.entities

-- | Unwrap Entity to get underlying EntityId.
-- |
-- | Useful for internal operations and debugging.
unEntity :: forall r. Entity r -> EntityId
unEntity (Entity entityId) = entityId

-- | Wrap EntityId in Entity phantom type.
-- |
-- | UNSAFE: Caller must ensure row type is correct!
-- | Only use internally where component presence is guaranteed.
wrapEntity :: forall r. EntityId -> Entity r
wrapEntity = Entity

-- | Get or create empty archetype for newly spawned entities.
-- |
-- | The empty archetype ("") holds entities with no components.
getOrCreateEmptyArchetype :: Map ArchetypeId Archetype -> Archetype
getOrCreateEmptyArchetype archetypes =
  case Map.lookup emptyArchetypeId archetypes of
    Just arch -> arch
    Nothing -> { entities: [], entityPositions: Map.empty, mask: emptyMask, labels: Set.empty, storage: CS.empty }

-- | Get the bit position for a component label.
-- |
-- | Returns Nothing if the label hasn't been registered yet.
getComponentMask :: String -> World -> Maybe Int
getComponentMask label world =
  Map.lookup label world.componentRegistry.labelToBit

-- | Get or create a bit position for a component label.
-- |
-- | If the label already exists, returns its existing bit.
-- | Otherwise, assigns the next available bit and updates the registry.
-- |
-- | Returns the bit position and updated world.
getOrCreateComponentMask :: String -> World -> { bit :: Int, world :: World }
getOrCreateComponentMask label world =
  case Map.lookup label world.componentRegistry.labelToBit of
    Just bit -> { bit, world }
    Nothing ->
      let
        bit = world.componentRegistry.nextBit
        newRegistry =
          { labelToBit: Map.insert label bit world.componentRegistry.labelToBit
          , nextBit: bit + 1
          }
      in
        { bit, world: world { componentRegistry = newRegistry } }

-- | Create a bitmask from a single bit position.
-- |
-- | Examples:
-- |   bitToMask 3  = ComponentMask [8]
-- |   bitToMask 32 = ComponentMask [0, 1]
bitToMask :: Int -> ComponentMask
bitToMask bit = maskAddBit emptyMask bit

-- | Check if `archMask` contains every bit set in `requiredMask`.
-- |
-- | Implemented word-by-word. If `requiredMask` extends beyond `archMask`
-- | (later words exist on the required side), the missing arch words are
-- | treated as zero and the predicate fails iff any required word is non-zero.
maskContains :: ComponentMask -> ComponentMask -> Boolean
maskContains (ComponentMask arch) (ComponentMask req) =
  go 0
  where
    nReq = Array.length req
    go i
      | i >= nReq = true
      | otherwise =
          let r = fromMaybe 0 (Array.index req i)
              a = fromMaybe 0 (Array.index arch i)
          in if (a .&. r) == r then go (i + 1) else false

-- | Check if `archMask` shares any bits with `excludedMask`.
-- |
-- | Implemented word-by-word over the overlap. Missing words on either side
-- | are zero and contribute nothing.
maskHasAny :: ComponentMask -> ComponentMask -> Boolean
maskHasAny (ComponentMask arch) (ComponentMask exc) =
  go 0
  where
    nOverlap = min (Array.length arch) (Array.length exc)
    go i
      | i >= nOverlap = false
      | otherwise =
          let a = fromMaybe 0 (Array.index arch i)
              e = fromMaybe 0 (Array.index exc i)
          in if (a .&. e) /= 0 then true else go (i + 1)

-- | Add a bit to a mask. Extends the underlying word array on demand;
-- | the result is already canonical (the just-set bit guarantees the
-- | top word is non-zero).
maskAddBit :: ComponentMask -> Int -> ComponentMask
maskAddBit (ComponentMask ws) bit =
  let
    wordIx = bit `div` 32
    bitInWord = bit `mod` 32
    bitValue = 1 `shl` bitInWord
    extended = padToLength (wordIx + 1) ws
    cur = fromMaybe 0 (Array.index extended wordIx)
    updated = fromMaybe extended (Array.updateAt wordIx (cur .|. bitValue) extended)
  in
    ComponentMask updated

-- | Remove a bit from a mask. Re-canonicalises by trimming trailing zero
-- | words so `Eq`/`Ord` remain in agreement with the set of bits.
maskRemoveBit :: ComponentMask -> Int -> ComponentMask
maskRemoveBit (ComponentMask ws) bit =
  let
    wordIx = bit `div` 32
    bitInWord = bit `mod` 32
    bitValue = 1 `shl` bitInWord
  in
    case Array.index ws wordIx of
      Nothing -> ComponentMask ws  -- bit was already zero (word doesn't exist)
      Just cur ->
        if (cur .&. bitValue) == 0
          then ComponentMask ws  -- bit was already zero
          else
            let
              cleared = cur .&. complement bitValue
              updated = fromMaybe ws (Array.updateAt wordIx cleared ws)
            in
              ComponentMask (trimTrailingZeros updated)

-- | Pad an array of Ints to at least `n` elements with trailing zeros.
padToLength :: Int -> Array Int -> Array Int
padToLength n ws =
  let cur = Array.length ws
  in if cur >= n then ws else ws <> Array.replicate (n - cur) 0

-- | Drop trailing zero words to keep canonical form (so `Eq`/`Ord` track
-- | the set of set bits, not the representation length).
trimTrailingZeros :: Array Int -> Array Int
trimTrailingZeros ws =
  case Array.last ws of
    Just 0 -> trimTrailingZeros (Array.take (Array.length ws - 1) ws)
    _      -> ws

-- | Create a cache key from required and excluded masks.
-- |
-- | Tuple keys avoid string allocation on every cache lookup.
makeQueryCacheKey :: ComponentMask -> ComponentMask -> QueryCacheKey
makeQueryCacheKey requiredMask excludedMask = Tuple requiredMask excludedMask

-- | Increment the structural version (invalidates query cache).
-- |
-- | Called when archetypes are created or modified structurally.
-- | Does NOT clear the cache - stale entries are detected by version check.
incrementStructuralVersion :: World -> World
incrementStructuralVersion world =
  world { structuralVersion = world.structuralVersion + 1 }


-- Note: Using Tuple syntax for State monad pattern matching
-- Pattern: let (Tuple state value) = runState action initialState
