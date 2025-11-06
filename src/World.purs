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
  , emptyWorld
  , spawnEntity
  , despawnEntity
  , hasEntity
  , unEntity
  , wrapEntity
  ) where

import Prelude

import Control.Monad.State (runState)
import Data.Array (findIndex, index, length, take, updateAt)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import ECS.Entity (EntityId, EntityManager, createEntity, deleteEntity, emptyEntityManager, entityIndex, validateEntity)
import ECS.Internal.ComponentStorage (ComponentStorage)
import ECS.Internal.ComponentStorage as CS

-- | Archetype ID derived from sorted component type names.
-- |
-- | Example: "Position,Velocity" or "Health,Position,Velocity"
-- | Components are sorted alphabetically for consistency.
type ArchetypeId = String

-- | World contains all ECS state.
-- |
-- | Structure:
-- | - entities: EntityManager for ID lifecycle (Phase 1)
-- | - archetypes: Type-erased archetype storage (Map ArchetypeId Foreign)
-- | - entityLocations: Tracks which archetype contains each entity
type World =
  { entities :: EntityManager
  , archetypes :: Map ArchetypeId Archetype  -- No more Foreign!
  , entityLocations :: Map Int ArchetypeId
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
-- | - storage: Component label -> Array Foreign (component values)
-- |
-- | Type safety is enforced at the Entity level via phantom row types,
-- | not at the archetype storage level. All archetypes have the same type.
type Archetype =
  { entities :: Array EntityId
  , storage :: ComponentStorage
  }

-- | Empty archetype ID for newly spawned entities.
emptyArchetypeId :: ArchetypeId
emptyArchetypeId = ""

-- | Create an empty world.
-- |
-- | Initial state:
-- | - No entities
-- | - No archetypes (will create "" archetype on first spawn)
-- | - Empty entity location tracking
emptyWorld :: World
emptyWorld =
  { entities: emptyEntityManager
  , archetypes: Map.empty
  , entityLocations: Map.empty
  }

-- | Spawn a new entity (empty, no components yet).
-- |
-- | Algorithm:
-- | 1. Generate EntityId via EntityManager
-- | 2. Find or create empty archetype ("")
-- | 3. Add EntityId to empty archetype
-- | 4. Update entityLocations map
-- | 5. Return Entity () with empty row type
-- |
-- | Returns both updated World and the new Entity handle.
spawnEntity :: World -> { world :: World, entity :: Entity () }
spawnEntity world =
  let
    -- Step 1: Create EntityId (see CLAUDE.md State monad pattern)
    (Tuple entityId state) = runState createEntity world.entities

    -- Step 2: Get or create empty archetype
    emptyArch = getOrCreateEmptyArchetype world.archetypes

    -- Step 3: Add entity to empty archetype
    updatedArch = emptyArch { entities = emptyArch.entities <> [entityId] }
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

-- | Despawn an entity (remove from world).
-- |
-- | Algorithm:
-- | 1. Unwrap Entity to get EntityId
-- | 2. Validate entity exists
-- | 3. Find entity's archetype via entityLocations
-- | 4. Remove entity from archetype (swap-remove)
-- | 5. Delete from entityLocations
-- | 6. Delete EntityId via EntityManager
-- | 7. Return updated World
despawnEntity :: forall r. Entity r -> World -> World
despawnEntity (Entity entityId) world =
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
                  -- Find entity's position in archetype
                  maybePos = findIndex (\e -> e == entityId) arch.entities

                  -- Swap-remove entity from archetype and remove its components
                  updatedArch = case maybePos of
                    Nothing -> arch  -- Entity not found, leave archetype unchanged
                    Just pos ->
                      let
                        -- Remove entity from entities array
                        updatedEntities = removeEntity entityId arch.entities

                        -- Remove corresponding components from all component arrays at index pos
                        updatedStorage = CS.mapWithKey (\_ arr ->
                          -- Remove element at index pos using filter with index
                          filterWithIndex (\i _ -> i /= pos) arr
                        ) arch.storage
                      in
                        { entities: updatedEntities, storage: updatedStorage }

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
    Nothing -> { entities: [], storage: CS.empty }


-- | Remove entity from archetype's entity array (swap-remove).
-- |
-- | Algorithm:
-- | 1. Find entity's position in array
-- | 2. If found, swap with last entity
-- | 3. Pop last element
-- | 4. Return updated array
-- |
-- | Complexity: O(n) for find, O(1) for remove
removeEntity :: EntityId -> Array EntityId -> Array EntityId
removeEntity targetId entities =
  case findIndex (\e -> e == targetId) entities of
    Nothing -> entities  -- Not found, return unchanged
    Just idx ->
      let
        lastIdx = length entities - 1
      in
        if idx == lastIdx then
          -- Target is last element, just pop
          take lastIdx entities
        else
          -- Swap with last, then pop
          let
            swapped = fromMaybe entities $ do
              lastElem <- index entities lastIdx
              updated <- updateAt idx lastElem entities
              pure updated
          in
            take lastIdx swapped

-- | Filter array with index predicate.
-- |
-- | Similar to filter but provides the index to the predicate function.
filterWithIndex :: forall a. (Int -> a -> Boolean) -> Array a -> Array a
filterWithIndex f arr = go 0 []
  where
    go idx acc =
      if idx >= length arr then acc
      else case index arr idx of
        Nothing -> go (idx + 1) acc
        Just x -> if f idx x
                   then go (idx + 1) (acc <> [x])
                   else go (idx + 1) acc

-- Note: Using Tuple syntax for State monad pattern matching
-- Pattern: let (Tuple state value) = runState action initialState
