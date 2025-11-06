-- | ECS Phase 1: Entity Foundation with Versioned IDs
-- |
-- | This module implements type-safe entity ID generation with generational
-- | indices and free-list recycling. The versioned IDs prevent use-after-free
-- | bugs by tracking entity lifecycles.
-- |
-- | Key concepts:
-- | - EntityId: 64-bit ID with index (32 bits) and version (32 bits)
-- | - EntityManager: Tracks entity lifecycle with free-list recycling
-- | - Versioning: Prevents stale references from accessing wrong entities
module ECS.Entity
  ( EntityId(..)
  , EntityManager
  , emptyEntityManager
  , createEntity
  , deleteEntity
  , validateEntity
  , entityCount
  , getEntityVersion
  , maxEntityId
  , entityIndex
  , entityVersion
  ) where

import Prelude

import Control.Monad.State (State, get, put)
import Data.List (List(..), length, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)

-- | Entity ID with generational index.
-- |
-- | Structure:
-- | - index: Position in entity arrays (enables O(1) lookups)
-- | - version: Generation counter (increments on deletion)
-- |
-- | Example lifecycle:
-- | 1. Create: {index: 5, version: 0}
-- | 2. Delete: version increments to 1
-- | 3. Recreate: {index: 5, version: 1}
-- | 4. Old ref {index: 5, version: 0} fails validation ✅
newtype EntityId = EntityId
  { index :: Int      -- Array index for fast lookup
  , version :: Int    -- Generation counter (prevents stale refs)
  }

derive instance eqEntityId :: Eq EntityId
derive instance ordEntityId :: Ord EntityId
derive newtype instance showEntityId :: Show EntityId

-- | Entity manager holds generation state.
-- |
-- | Invariants:
-- | - nextIndex is monotonically increasing (never decrements)
-- | - freeList contains only deleted entity indices
-- | - versions map contains current version for each index
-- | - All indices in freeList have entries in versions map
type EntityManager =
  { nextIndex :: Int              -- Next new index to allocate
  , freeList :: List Int          -- Indices available for reuse (LIFO stack)
  , versions :: Map Int Int       -- index -> current version
  }

-- | Create an empty entity manager.
-- |
-- | Initial state:
-- | - nextIndex: 0 (no entities allocated yet)
-- | - freeList: empty (no deleted entities)
-- | - versions: empty (no version tracking yet)
emptyEntityManager :: EntityManager
emptyEntityManager =
  { nextIndex: 0
  , freeList: Nil
  , versions: Map.empty
  }

-- | Generate a new entity ID (pure State monad).
-- |
-- | Algorithm:
-- | 1. Check if freeList has entries
-- | 2. If yes: pop from freeList, increment version
-- | 3. If no: use nextIndex, version = 0
-- | 4. Return EntityId {index, version}
-- |
-- | Complexity: O(log n) for Map operations
createEntity :: State EntityManager EntityId
createEntity = do
  manager <- get
  case manager.freeList of
    -- Free list has recycled indices - reuse them (LIFO)
    -- Version was already incremented in deleteEntity, so just use current version
    (idx : rest) -> do
      let currentVersion = fromMaybe 0 (Map.lookup idx manager.versions)
      put manager
        { freeList = rest
        , versions = manager.versions  -- Keep current versions
        }
      pure $ EntityId { index: idx, version: currentVersion }

    -- No recycled indices - allocate a new one
    Nil -> do
      let idx = manager.nextIndex
          newVersions = Map.insert idx 0 manager.versions
      put manager
        { nextIndex = manager.nextIndex + 1
        , versions = newVersions
        }
      pure $ EntityId { index: idx, version: 0 }

-- | Mark entity as deleted (returns to free list).
-- |
-- | Algorithm:
-- | 1. Validate entity exists with matching version
-- | 2. Increment version in versions map
-- | 3. Push index onto freeList (LIFO)
-- | 4. Return Unit
-- |
-- | Idempotent: Deleting already-deleted entity does nothing.
-- |
-- | Complexity: O(log n) for Map operations
deleteEntity :: EntityId -> State EntityManager Unit
deleteEntity (EntityId entity) = do
  manager <- get
  -- Only delete if entity is currently valid
  if validateEntity (EntityId entity) manager then do
    let currentVersion = fromMaybe 0 (Map.lookup entity.index manager.versions)
        newVersion = currentVersion + 1
        newVersions = Map.insert entity.index newVersion manager.versions
        newFreeList = entity.index : manager.freeList
    put manager
      { freeList = newFreeList
      , versions = newVersions
      }
  else
    -- Entity already deleted or invalid - do nothing (idempotent)
    pure unit

-- | Check if entity ID is currently valid.
-- |
-- | Algorithm:
-- | 1. Lookup index in versions map
-- | 2. Compare stored version with EntityId version
-- | 3. Return true if matches, false otherwise
-- |
-- | Returns false if:
-- | - Index doesn't exist in versions map
-- | - Stored version doesn't match EntityId version
-- |
-- | Complexity: O(log n) for Map lookup
validateEntity :: EntityId -> EntityManager -> Boolean
validateEntity (EntityId entity) manager =
  case Map.lookup entity.index manager.versions of
    Just currentVersion -> currentVersion == entity.version
    Nothing -> false

-- | Get the current version for an entity index.
-- |
-- | Useful for debugging and inspection.
getEntityVersion :: Int -> EntityManager -> Maybe Int
getEntityVersion idx manager = Map.lookup idx manager.versions

-- | Count of live entities (allocated but not in free list).
-- |
-- | Formula: nextIndex - length(freeList)
-- |
-- | This represents entities that are currently "alive" and not deleted.
entityCount :: EntityManager -> Int
entityCount manager = manager.nextIndex - length manager.freeList

-- | Maximum entity ID ever allocated.
-- |
-- | Returns: nextIndex - 1 (or -1 if no entities)
-- |
-- | This is the highest index that has been allocated, regardless of
-- | whether it's currently alive or deleted.
maxEntityId :: EntityManager -> Int
maxEntityId manager = manager.nextIndex - 1

-- | Extract the index from an EntityId.
entityIndex :: EntityId -> Int
entityIndex (EntityId e) = e.index

-- | Extract the version from an EntityId.
entityVersion :: EntityId -> Int
entityVersion (EntityId e) = e.version
