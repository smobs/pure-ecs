-- | ECS.Debug — human-readable World snapshots for diagnosis.
-- |
-- | The `dumpWorld` helper produces a multi-line String summarising the World's
-- | structural state: component registry, archetypes (mask + labels + storage
-- | columns), entity-location count, and query-cache freshness. Inline
-- | divergence markers flag cases where an archetype's mask, labels Set, and
-- | storage columns disagree — i.e. the kinds of corruption that ad-hoc
-- | console.log sessions tend to uncover the hard way.
-- |
-- | This is a debug tool, not part of the hot path. It walks every archetype
-- | and every storage column, so don't call it inside a frame.
module ECS.Debug
  ( dumpWorld
  ) where

import Prelude

import Data.Array (foldl, length, sort, sortBy) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import ECS.Internal.ComponentStorage as CS
import ECS.World (Archetype, ComponentMask, World, popcountMask)

-- | Render a `World` as a multi-line, human-readable text dump.
-- |
-- | Sections (in order):
-- |   1. Header line with totals.
-- |   2. ComponentRegistry — labelToBit pairs sorted by bit position.
-- |   3. Archetypes — each archetype's mask, labels, storage columns, with
-- |      inline `‼ DIVERGENCE` markers when invariants disagree.
-- |   4. EntityLocations — count only (entity-by-entity is too noisy).
-- |   5. QueryCache — fresh vs stale entry counts.
dumpWorld :: World -> String
dumpWorld world =
  joinLines
    $ [ header world ]
    <> registrySection world
    <> archetypesSection world
    <> entityLocationsSection world
    <> queryCacheSection world

-- =============================================================================
-- Sections
-- =============================================================================

header :: World -> String
header world =
  let
    entityCount = Map.size world.entityLocations
    archCount = Map.size world.archetypes
    labelCount = Map.size world.componentRegistry.labelToBit
    wordCount =
      if world.componentRegistry.nextBit == 0
        then 0
        else (world.componentRegistry.nextBit - 1) `div` 32 + 1
  in
    "World: " <> show entityCount <> " entities, "
      <> show archCount <> " archetypes, "
      <> show labelCount <> " registered labels ("
      <> show wordCount <> " words)"

registrySection :: World -> Array String
registrySection world =
  [ "ComponentRegistry:" ]
    <> map renderEntry sortedByBit
  where
    sortedByBit =
      Array.sortBy (\(Tuple _ b1) (Tuple _ b2) -> compare b1 b2)
        (mapToTuples world.componentRegistry.labelToBit)
    renderEntry (Tuple label bit) =
      "  bit " <> show bit <> ": \"" <> label <> "\""

archetypesSection :: World -> Array String
archetypesSection world =
  [ "Archetypes:" ]
    <> Array.foldl (\acc (Tuple m arch) -> acc <> renderArchetype m arch) [] sortedArchs
  where
    sortedArchs =
      Array.sortBy (\(Tuple m1 _) (Tuple m2 _) -> compare m1 m2)
        (mapToTuples world.archetypes)

renderArchetype :: ComponentMask -> Archetype -> Array String
renderArchetype mask arch =
  let
    labelsSorted = Array.sort (setToArray arch.labels)
    pc = popcountMask mask
    columns = storageColumns arch.storage
    columnsSorted =
      Array.sortBy (\(Tuple a _) (Tuple b _) -> compare a b) columns
    columnNames = Set.fromFoldable (map fstTuple columns)
    labelsNotInColumns = Set.difference arch.labels columnNames
    columnsNotInLabels = Set.difference columnNames arch.labels
    sizeMismatch = Set.size arch.labels /= pc
    divergent =
      sizeMismatch
        || not (Set.isEmpty labelsNotInColumns)
        || not (Set.isEmpty columnsNotInLabels)
    base =
      [ "  - mask: " <> show mask
      , "    labels: " <> show labelsSorted <> " (popcount=" <> show pc <> ")"
      , "    entities: " <> show (Array.length arch.entities)
      , "    storage columns:"
      ]
        <> map renderColumn columnsSorted
    divergenceLines =
      [ "    \x203C DIVERGENCE"
      , "        |labels|=" <> show (Set.size arch.labels)
          <> " popcount(mask)=" <> show pc
      , "        labels \\ columns: "
          <> show (Array.sort (setToArray labelsNotInColumns))
      , "        columns \\ labels: "
          <> show (Array.sort (setToArray columnsNotInLabels))
      ]
  in
    if divergent then base <> divergenceLines else base

renderColumn :: Tuple String Int -> String
renderColumn (Tuple name len) =
  "      " <> name <> ": " <> show len

storageColumns :: CS.ComponentStorage -> Array (Tuple String Int)
storageColumns storage =
  CS.fold (\acc label arr -> acc <> [ Tuple label (Array.length arr) ]) [] storage

entityLocationsSection :: World -> Array String
entityLocationsSection world =
  [ "EntityLocations:"
  , "  " <> show (Map.size world.entityLocations) <> " entities mapped"
  ]

queryCacheSection :: World -> Array String
queryCacheSection world =
  let
    cv = world.structuralVersion
    counts =
      Array.foldl tally (Tuple 0 0)
        (mapToTuples world.queryCache)
    tally (Tuple f s) (Tuple _ entry) =
      if entry.version == cv
        then Tuple (f + 1) s
        else Tuple f (s + 1)
    fresh = case counts of Tuple f _ -> f
    stale = case counts of Tuple _ s -> s
  in
    [ "QueryCache:"
    , "  fresh=" <> show fresh <> " stale=" <> show stale
        <> " (structuralVersion=" <> show cv <> ")"
    ]

-- =============================================================================
-- Tiny helpers
-- =============================================================================

mapToTuples :: forall k v. Map k v -> Array (Tuple k v)
mapToTuples = Map.toUnfoldable

setToArray :: forall a. Set a -> Array a
setToArray = Set.toUnfoldable

fstTuple :: forall a b. Tuple a b -> a
fstTuple (Tuple a _) = a

joinLines :: Array String -> String
joinLines = Array.foldl step ""
  where
    step "" line = line
    step acc line = acc <> "\n" <> line
