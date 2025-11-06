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
  , forQuery
  , mapQuery
  , class ExtractLabels
  , extractLabels
  , class ReadComponents
  , readComponents
  ) where

import Prelude

import Data.Array (filter, foldl, index, uncons)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.Common (trim)
import Data.Tuple.Nested (type (/\), (/\))
import ECS.Entity (EntityId, entityIndex)
import ECS.World (World, Entity, ArchetypeId, Archetype, wrapEntity)
import ECS.Internal.ComponentStorage as CS
import Foreign (Foreign)
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
-- | 1. Iterate all archetypes in world
-- | 2. Check if archetype matches required/excluded constraints
-- | 3. For matching archetypes, extract all entities
-- | 4. For each entity, read component values using RowToList
-- | 5. Build QueryResult records
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
    -- Get all archetypes from world
    archetypes = Map.toUnfoldable world.archetypes :: Array _

    -- Filter to matching archetypes
    matchingArchs = filter (\(archId /\ _) -> archetypeMatches archId q.requiredLabels q.excludedLabels) archetypes

    -- Extract results from each matching archetype
    results = foldl (\acc (archId /\ arch) ->
      let archResults = extractFromArchetype archId arch world
      in acc <> archResults
    ) [] matchingArchs
  in results
  where
    -- Extract query results from a single archetype
    extractFromArchetype :: ArchetypeId -> Archetype -> World -> Array (QueryResult required)
    extractFromArchetype archId arch world' =
      let

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

-- | Check if an archetype matches query constraints.
-- |
-- | Matching rules:
-- | 1. Archetype MUST contain ALL required components (set containment)
-- | 2. Archetype MUST NOT contain ANY excluded components (set disjoint)
-- |
-- | Time complexity: O(R + E) where R = required count, E = excluded count
archetypeMatches :: ArchetypeId -> Set String -> Set String -> Boolean
archetypeMatches archId required excluded =
  let
    -- Parse archetype ID to set of component labels
    archLabels = parseArchetypeId archId

    -- Check all required components exist
    hasRequired = required `Set.subset` archLabels

    -- Check no excluded components exist
    noExcluded = Set.isEmpty (excluded `Set.intersection` archLabels)
  in
    hasRequired && noExcluded

-- | Parse archetype ID string to set of component labels.
-- |
-- | Example: "Position,Velocity" -> Set {"Position", "Velocity"}
parseArchetypeId :: ArchetypeId -> Set String
parseArchetypeId archId =
  if archId == "" then
    Set.empty
  else
    let parts = split (Pattern ",") archId
        trimmed = map trim parts
    in Set.fromFoldable trimmed

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
-- | 2. Find entity's index in archetype
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
        -- Find entity's index in archetype
        entityIdx = findEntityIndex entityId arch.entities
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

-- | Find entity's index in archetype's entity array.
findEntityIndex :: EntityId -> Array EntityId -> Maybe Int
findEntityIndex entityId entities =
  let
    idx = entityIndex entityId
  in
    -- Linear search for entity (could optimize with Map)
    findIndexHelper (\eid -> entityIndex eid == idx) entities
  where
    findIndexHelper :: forall a. (a -> Boolean) -> Array a -> Maybe Int
    findIndexHelper predicate arr = go 0 arr
      where
        go :: Int -> Array a -> Maybe Int
        go i xs = case uncons xs of
          Nothing -> Nothing
          Just { head: x, tail: rest } ->
            if predicate x then Just i
            else go (i + 1) rest
