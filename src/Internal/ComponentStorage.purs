-- | Internal module for safe component storage operations.
-- |
-- | This module encapsulates the Foreign-based storage implementation,
-- | providing a type-safe API that leverages our internal invariants.
-- |
-- | Invariant: All values in ComponentStorage are Array Foreign.
-- | This invariant is maintained by the smart constructors and operations.
module ECS.Internal.ComponentStorage
  ( ComponentStorage
  , ComponentArray
  , empty
  , lookup
  , insert
  , mapWithKey
  , fold
  , filterKeys
  -- Component array operations
  , emptyArray
  , arrayAppend
  , arrayIndex
  , arrayFromSingleton
  , arrayRemoveAt
  , componentToForeign
  , componentFromForeign
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Storage mapping component labels to component arrays.
-- |
-- | Invariant: Every value is an Array Foreign (component values).
-- | This is an opaque type - only this module can create/modify it.
newtype ComponentStorage = ComponentStorage (Object Foreign)

-- Manual Eq instance for ComponentStorage (Foreign doesn't have Eq)
-- We compare object keys only (structural equality on Foreign is unsafe)
instance eqComponentStorage :: Eq ComponentStorage where
  eq (ComponentStorage a) (ComponentStorage b) =
    let keysA = Array.sort $ Object.keys a
        keysB = Array.sort $ Object.keys b
    in keysA == keysB

-- | Type alias for clarity - component arrays are always Array Foreign
type ComponentArray = Array Foreign

-- | Create an empty component storage
empty :: ComponentStorage
empty = ComponentStorage Object.empty

-- | Lookup a component array by label
-- |
-- | Safe: We know all values are Array Foreign due to our invariant.
lookup :: String -> ComponentStorage -> Maybe ComponentArray
lookup label (ComponentStorage obj) =
  case Object.lookup label obj of
    Nothing -> Nothing
    Just foreignArray ->
      -- Safe: Our invariant guarantees this is Array Foreign
      Just (unsafeFromForeign foreignArray)

-- | Insert a component array at a label
-- |
-- | Maintains invariant: We wrap the Array Foreign in Foreign.
insert :: String -> ComponentArray -> ComponentStorage -> ComponentStorage
insert label arr (ComponentStorage obj) =
  ComponentStorage (Object.insert label (unsafeToForeign arr) obj)

-- | Map over storage with access to labels
-- |
-- | The mapper receives the actual ComponentArray (Array Foreign),
-- | maintaining type safety.
mapWithKey :: (String -> ComponentArray -> ComponentArray) -> ComponentStorage -> ComponentStorage
mapWithKey f (ComponentStorage obj) =
  ComponentStorage $ Object.mapWithKey (\label foreignArray ->
    let arr = unsafeFromForeign foreignArray :: ComponentArray
        newArr = f label arr
    in unsafeToForeign newArr
  ) obj

-- | Fold over component storage
-- |
-- | The folder receives label and the actual ComponentArray.
fold :: forall a. (a -> String -> ComponentArray -> a) -> a -> ComponentStorage -> a
fold f acc (ComponentStorage obj) =
  Object.fold (\a label foreignArr ->
    let arr = unsafeFromForeign foreignArr :: ComponentArray
    in f a label arr
  ) acc obj

-- | Filter storage by label predicate
filterKeys :: (String -> Boolean) -> ComponentStorage -> ComponentStorage
filterKeys predicate (ComponentStorage obj) =
  ComponentStorage (Object.filterKeys predicate obj)

-- =============================================================================
-- Component Array Operations
-- =============================================================================

-- | Create an empty component array
emptyArray :: ComponentArray
emptyArray = []

-- | Append a component value to an array
arrayAppend :: forall a. a -> ComponentArray -> ComponentArray
arrayAppend component arr =
  arr <> [unsafeToForeign component]

-- | Get component at index
-- |
-- | Returns the Foreign value directly (caller must unsafeFromForeign).
arrayIndex :: Int -> ComponentArray -> Maybe Foreign
arrayIndex idx arr = Array.index arr idx

-- | Create a component array from a single value
arrayFromSingleton :: forall a. a -> ComponentArray
arrayFromSingleton val = [unsafeToForeign val]

-- | Remove component at index (preserves order, uses filter)
arrayRemoveAt :: Int -> ComponentArray -> ComponentArray
arrayRemoveAt idx arr =
  Array.mapWithIndex (\i v -> {i, v}) arr
    # Array.filter (\{i} -> i /= idx)
    # map _.v

-- | Convert a component value to Foreign for storage
componentToForeign :: forall a. a -> Foreign
componentToForeign = unsafeToForeign

-- | Extract a component value from Foreign
-- |
-- | Unsafe: Caller must ensure the type matches.
componentFromForeign :: forall a. Foreign -> a
componentFromForeign = unsafeFromForeign
