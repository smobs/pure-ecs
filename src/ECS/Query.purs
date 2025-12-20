-- | ECS Phase 4: Row-Polymorphic Query System
-- |
-- | This module implements type-safe entity queries using PureScript's row
-- | polymorphism. Queries specify required and excluded components at compile-time,
-- | returning entities that match the component signature with preserved field names.
-- |
-- | Key concepts:
-- | - Query: Specifies required and excluded components using row types
-- | - QueryResult: Contains entity handle + typed component record
-- | - RowToList: Generic iteration over component labels
-- | - Field name preservation: Results have named fields (not c1, c2)
module ECS.Query
  ( Query
  , QueryResult
  , query
  , without
  , runQuery
  , runQueryCached
  , forQuery
  , mapQuery
  , class ExtractLabels
  , extractLabels
  , class ReadComponents
  , readComponents
  ) where

import Prelude

import Data.Array (foldl)
import Data.Foldable (foldl) as F
import Data.Int.Bits (shl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import ECS.Entity (EntityId, entityIndex)
import ECS.World (World, Entity, ArchetypeId, Archetype, ComponentMask, QueryCacheKey, CachedQueryResult, wrapEntity, maskContains, maskHasAny, makeQueryCacheKey)
import ECS.Internal.ComponentStorage as CS
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (class IsSymbol, reflectSymbol)

-- | Query data type with row parameters for required and excluded components.
-- |
-- | Type parameters:
-- | - required: Row of components that MUST exist (AND logic)
-- | - excluded: Row of components that MUST NOT exist (NOT logic)
-- |
-- | Example:
-- |   Query (position :: Position, velocity :: Velocity)
-- |        (frozen :: Frozen)
-- |
-- | Meaning: Entities with position AND velocity, but NOT frozen
data Query (required :: Row Type) (excluded :: Row Type) = Query
  { requiredLabels :: Set String
  , excludedLabels :: Set String
  }

-- | Query result contains the entity handle and its component values.
-- |
-- | The components field preserves field names from the query row type.
-- | This enables type-safe field access: result.components.position
type QueryResult (components :: Row Type) =
  { entity :: Entity components
  , components :: Record components
  }

-- | Create a query from a component type specification using Proxy.
-- |
-- | The Proxy carries only type information at compile-time.
-- | The labels are extracted at runtime to match against archetypes.
-- |
-- | Example:
-- |   query (Proxy :: _ (position :: Position, velocity :: Velocity))
-- |   -- Type: Query (position :: Position, velocity :: Velocity) ()
query :: forall r rl.
  RowToList r rl =>
  ExtractLabels rl =>
  Proxy r ->
  Query r ()
query _ =
  let labels = extractLabels (Proxy :: Proxy rl)
  in Query { requiredLabels: labels, excludedLabels: Set.empty }

-- | Add an exclusion constraint to a query.
-- |
-- | Type constraints ensure you can't exclude the same component twice.
-- |
-- | Example:
-- |   query { position: unit }
-- |     # without (Proxy :: Proxy "frozen")
-- |   -- Type: Query (position :: Unit) (frozen :: Unit)
without :: forall required excluded excluded' label typ.
  IsSymbol label =>
  Lacks label excluded =>
  Cons label typ excluded excluded' =>
  Proxy label ->
  Query required excluded ->
  Query required excluded'
without labelProxy (Query q) =
  let labelStr = reflectSymbol labelProxy
      newExcluded = Set.insert labelStr q.excludedLabels
  in Query { requiredLabels: q.requiredLabels, excludedLabels: newExcluded }

-- | Execute a query, returning all matching entities with their components.
-- |
-- | Algorithm:
-- | 1. Convert required/excluded labels to bitmasks
-- | 2. Iterate all archetypes in world
-- | 3. Check if archetype matches using O(1) bitmask operations
-- | 4. For matching archetypes, extract all entities
-- | 5. For each entity, read component values using RowToList
-- | 6. Build QueryResult records
-- |
-- | Returns: Array of query results
runQuery :: forall required excluded rl.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Query required excluded ->
  World ->
  Array (QueryResult required)
runQuery (Query q) world =
  let
    -- Convert labels to masks using the world's registry
    -- If any required label is not registered, no archetype can match
    requiredResult = labelsToMaskStrict q.requiredLabels world
    excludedMask = labelsToMask q.excludedLabels world
  in
    case requiredResult of
      Nothing -> []  -- Required component not registered, no matches possible
      Just requiredMask ->
        let
          -- Get all archetypes from world
          archetypes = Map.toUnfoldable world.archetypes :: Array _

          -- Filter to matching archetypes using O(1) bitmask operations
          matchingArchs = foldl (\acc (_ /\ arch) ->
            if archetypeMatchesMask arch.mask requiredMask excludedMask
              then acc <> [arch]
              else acc
          ) [] archetypes

          -- Extract results from each matching archetype
          results = foldl (\acc arch ->
            let archResults = extractFromArchetype arch world
            in acc <> archResults
          ) [] matchingArchs
        in results
  where
    -- Extract query results from a single archetype
    extractFromArchetype :: Archetype -> World -> Array (QueryResult required)
    extractFromArchetype arch world' =
      let
        -- Find archetype ID for reading (needed by readComponents)
        archId = findArchetypeId arch world'

        -- Build result for each entity in archetype
        entityResults = map (\entityId ->
          let
            -- Read components for this entity
            components = readComponents (Proxy :: Proxy rl) archId entityId world'
          in
            { entity: wrapEntity entityId
            , components
            }
        ) arch.entities
      in entityResults

    -- Find archetype ID from archetype (reverse lookup)
    findArchetypeId :: Archetype -> World -> ArchetypeId
    findArchetypeId arch w =
      let
        archs = Map.toUnfoldable w.archetypes :: Array _
        found = foldl (\acc (id /\ a) ->
          if a.mask == arch.mask then id else acc
        ) "" archs
      in found

-- | Convert a set of labels to a bitmask using the world's registry.
-- | Returns Nothing if any label is not registered (meaning no matches possible).
labelsToMaskStrict :: Set String -> World -> Maybe ComponentMask
labelsToMaskStrict labels world =
  F.foldl (\maybeAcc label ->
    case maybeAcc of
      Nothing -> Nothing  -- Already failed
      Just acc ->
        case Map.lookup label world.componentRegistry.labelToBit of
          Just bit -> Just (acc + (1 `shl` bit))
          Nothing -> Nothing  -- Label not registered, no matches possible
  ) (Just 0) labels

-- | Convert a set of labels to a bitmask using the world's registry.
-- | Labels not found are ignored (used for exclusion where missing = no exclusion).
labelsToMask :: Set String -> World -> ComponentMask
labelsToMask labels world =
  F.foldl (\mask label ->
    case Map.lookup label world.componentRegistry.labelToBit of
      Just bit -> mask + (1 `shl` bit)
      Nothing -> mask  -- Label not registered, treat as 0
  ) 0 labels

-- | Check if archetype matches query using O(1) bitmask operations.
-- |
-- | Matching rules:
-- | 1. Archetype MUST have ALL required bits set
-- | 2. Archetype MUST NOT have ANY excluded bits set
archetypeMatchesMask :: ComponentMask -> ComponentMask -> ComponentMask -> Boolean
archetypeMatchesMask archMask requiredMask excludedMask =
  maskContains archMask requiredMask &&
  not (maskHasAny archMask excludedMask)

-- | Execute a query with caching, returning results and updated world.
-- |
-- | This version caches the list of matching archetype IDs. When the same
-- | query is run again and the world's structural version hasn't changed,
-- | it skips archetype matching and uses the cached list.
-- |
-- | Cache invalidation: The cache is invalidated when structuralVersion changes,
-- | which happens when new archetypes are created.
-- |
-- | Returns: Query results and the world (with updated cache if needed)
runQueryCached :: forall required excluded rl.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Query required excluded ->
  World ->
  { results :: Array (QueryResult required), world :: World }
runQueryCached (Query q) world =
  let
    -- Convert labels to masks
    requiredResult = labelsToMaskStrict q.requiredLabels world
    excludedMask = labelsToMask q.excludedLabels world
  in
    case requiredResult of
      Nothing -> { results: [], world }  -- Required component not registered
      Just requiredMask ->
        let
          -- Create cache key
          cacheKey = makeQueryCacheKey requiredMask excludedMask

          -- Check cache
          cacheResult = checkCache cacheKey world

          -- Get matching archetype IDs (from cache or compute)
          { matchingArchIds, world': worldAfterCache } = case cacheResult of
            Just archIds ->
              -- Cache hit! Use cached archetype IDs
              { matchingArchIds: archIds, world': world }
            Nothing ->
              -- Cache miss - compute and cache
              let
                archetypes = Map.toUnfoldable world.archetypes :: Array _
                archIds = foldl (\acc (archId /\ arch) ->
                  if archetypeMatchesMask arch.mask requiredMask excludedMask
                    then acc <> [archId]
                    else acc
                ) [] archetypes
                -- Update cache
                newCacheEntry = { matchingArchetypes: archIds, version: world.structuralVersion }
                updatedCache = Map.insert cacheKey newCacheEntry world.queryCache
              in
                { matchingArchIds: archIds
                , world': world { queryCache = updatedCache }
                }

          -- Extract results from matching archetypes
          results = foldl (\acc archId ->
            case Map.lookup archId worldAfterCache.archetypes of
              Nothing -> acc  -- Archetype doesn't exist (shouldn't happen with valid cache)
              Just arch ->
                let archResults = extractFromArchetypeCached archId arch worldAfterCache
                in acc <> archResults
          ) [] matchingArchIds
        in
          { results, world: worldAfterCache }
  where
    -- Check if we have a valid cached result
    checkCache :: QueryCacheKey -> World -> Maybe (Array ArchetypeId)
    checkCache key w =
      case Map.lookup key w.queryCache of
        Nothing -> Nothing
        Just cached ->
          if cached.version == w.structuralVersion
            then Just cached.matchingArchetypes
            else Nothing  -- Stale cache entry

    -- Extract query results from a single archetype (using archetype ID directly)
    extractFromArchetypeCached :: ArchetypeId -> Archetype -> World -> Array (QueryResult required)
    extractFromArchetypeCached archId arch world' =
      map (\entityId ->
        let components = readComponents (Proxy :: Proxy rl) archId entityId world'
        in { entity: wrapEntity entityId, components }
      ) arch.entities

-- | Apply a callback to each query result, collecting the return values.
-- |
-- | Example:
-- |   forQuery movementQuery (\r -> r.components.position.x) world
-- |   -- Returns: Array Number (all x positions)
forQuery :: forall required excluded rl a.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Query required excluded ->
  (QueryResult required -> a) ->
  World ->
  Array a
forQuery q f world =
  let results = runQuery q world
  in map f results

-- | Map a query, updating the world for each result.
-- |
-- | The callback receives each result and the current world state,
-- | returning an updated world. Updates are threaded through sequentially.
-- |
-- | Example:
-- |   mapQuery movementQuery updatePosition world
-- |   where updatePosition r w = -- Update position in w
mapQuery :: forall required excluded rl.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Query required excluded ->
  (QueryResult required -> World -> World) ->
  World ->
  World
mapQuery q f world =
  let results = runQuery q world
  in foldl (\w r -> f r w) world results

-- ============================================================================
-- Type Classes for Generic Row Operations
-- ============================================================================

-- | Extract component labels from a row type at compile-time.
-- |
-- | This type class walks the RowList at compile-time, building a Set of
-- | label strings at runtime. Used for converting row types to archetype IDs.
class ExtractLabels (rl :: RowList Type) where
  extractLabels :: Proxy rl -> Set String

instance extractLabelsNil :: ExtractLabels RL.Nil where
  extractLabels _ = Set.empty

instance extractLabelsCons ::
  ( IsSymbol label
  , ExtractLabels tail
  ) =>
  ExtractLabels (RL.Cons label typ tail) where
  extractLabels _ =
    let
      labelStr = reflectSymbol (Proxy :: Proxy label)
      restLabels = extractLabels (Proxy :: Proxy tail)
    in Set.insert labelStr restLabels

-- | Read components from archetype storage into a typed record.
-- |
-- | This type class walks the RowList, reading each component from the
-- | archetype's storage and building up a record with proper field names.
class ReadComponents (rl :: RowList Type) (r :: Row Type) | rl -> r where
  readComponents :: Proxy rl -> ArchetypeId -> EntityId -> World -> Record r

instance readComponentsNil :: ReadComponents RL.Nil () where
  readComponents _ _ _ _ = {}

instance readComponentsCons ::
  ( IsSymbol label
  , Cons label typ r' r
  , Lacks label r'
  , ReadComponents tail r'
  ) =>
  ReadComponents (RL.Cons label typ tail) r where
  readComponents _ archId entityId world =
    let
      -- Read the current component
      labelStr = reflectSymbol (Proxy :: Proxy label)
      componentValue = readComponentValue labelStr archId entityId world

      -- Read the rest of the components
      rest = readComponents (Proxy :: Proxy tail) archId entityId world

      -- Insert this component into the record
      result = Record.insert (Proxy :: Proxy label) componentValue rest
    in result

-- | Read a single component value from archetype storage.
-- |
-- | Algorithm:
-- | 1. Find archetype in world
-- | 2. Find entity's index in archetype using O(log N) position map
-- | 3. Get component array for the label
-- | 4. Index into array at entity's position
-- | 5. Unsafely coerce from Foreign to component type
-- |
-- | Safety: Type erasure is controlled - we only insert typed values
-- | and retrieve them with the same types via row polymorphism.
readComponentValue :: forall a. String -> ArchetypeId -> EntityId -> World -> a
readComponentValue label archId entityId world =
  case Map.lookup archId world.archetypes of
    Nothing ->
      -- Archetype doesn't exist (shouldn't happen in valid query)
      CS.componentFromForeign (CS.componentToForeign unit)

    Just arch ->
      let
        -- Find entity's index in archetype using O(log N) position map
        entityIdx = Map.lookup (entityIndex entityId) arch.entityPositions
      in
        case entityIdx of
          Nothing ->
            -- Entity not in archetype (shouldn't happen)
            CS.componentFromForeign (CS.componentToForeign unit)

          Just idx ->
            case CS.lookup label arch.storage of
              Nothing ->
                -- Component doesn't exist (shouldn't happen if archetype matches)
                CS.componentFromForeign (CS.componentToForeign unit)

              Just componentArray ->
                case CS.arrayIndex idx componentArray of
                  Nothing ->
                    -- Index out of bounds (shouldn't happen)
                    CS.componentFromForeign (CS.componentToForeign unit)

                  Just foreignValue ->
                    -- Cast from Foreign to component type
                    CS.componentFromForeign foreignValue
