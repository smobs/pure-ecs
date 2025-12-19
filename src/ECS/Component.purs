-- | ECS Phase 3: Component Operations with Row Constraints
-- |
-- | This module implements type-safe component add/remove/get operations using
-- | PureScript's row polymorphism. Lacks and Cons constraints enforce compile-time
-- | safety, preventing duplicate components and ensuring components exist before access.
-- |
-- | Key concepts:
-- | - Lacks: Proves component doesn't exist (for safe addition)
-- | - Cons: Proves component exists (for safe removal/access)
-- | - Archetype migration: Moving entities when components change
-- | - Row type tracking: Phantom types track components at compile-time
module ECS.Component
  ( addComponent
  , removeComponent
  , getComponent
  , hasComponent
  -- Chaining combinator
  , with
  , (<+>)
  -- Component pairing for elegant syntax
  , ComponentPair(..)
  , (:=)
  -- Pure versions (for internal use)
  , addComponentPure
  , removeComponentPure
  , getComponentPure
  ) where

import Prelude

import Control.Monad.State (State, state, get)
import Data.Array (elem, filter, findIndex, index, length, sort, take, updateAt, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Common (trim)
import Data.Tuple (Tuple(..))
import ECS.Entity (EntityId, entityIndex, validateEntity)
import ECS.World (World, Entity, ArchetypeId, ComponentMask, unEntity, wrapEntity, getOrCreateComponentMask, maskAddBit, maskRemoveBit)
import ECS.Internal.ComponentStorage (ComponentStorage, arraySwapRemoveAt)
import ECS.Internal.ComponentStorage as CS
import Foreign (Foreign)
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol, reflectSymbol)

-- | Add a component to an entity (monadic version).
-- |
-- | This is the main API. It works within the State monad over World.
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Lacks label r: Proves component doesn't already exist (prevents duplicates)
-- | - Cons label a r r': Adding component produces new row type r'
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   e' <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |   e'' <- addComponent (Proxy :: _ "velocity") {x: 1.0, y: 1.0} e'
-- |   pure e''
-- | ```
addComponent :: forall r r' label a.
  IsSymbol label =>
  Lacks label r =>
  Cons label a r r' =>
  Proxy label ->
  a ->
  Entity r ->
  State World (Entity r')
addComponent labelProxy componentValue entity = state \world ->
  let result = addComponentPure labelProxy componentValue entity world
  in Tuple result.entity result.world

-- | Component label-value pair for elegant chaining syntax.
-- |
-- | Used with the `:=` operator to create readable component additions:
-- | ```purescript
-- | entity <- spawnEntity
-- |   <+> (Proxy :: _ "position") := {x: 0.0, y: 0.0}
-- |   <+> (Proxy :: _ "velocity") := {x: 1.0, y: 1.0}
-- | ```
data ComponentPair label a = ComponentPair (Proxy label) a

-- | Infix operator for pairing component label with value.
-- |
-- | Reads naturally: "position has value {x: 0.0, y: 0.0}"
-- |
-- | Example:
-- | ```purescript
-- | (Proxy :: _ "position") := {x: 0.0, y: 0.0}
-- | ```
infixr 6 ComponentPair as :=

-- | Chaining combinator for adding components without manual entity threading.
-- |
-- | This combinator prevents stale entity reference bugs by automatically
-- | threading the entity through component additions.
-- |
-- | Example:
-- | ```purescript
-- | entity <- spawnEntity
-- |   <+> (Proxy :: _ "position") := {x: 0.0, y: 0.0}
-- |   <+> (Proxy :: _ "velocity") := {x: 1.0, y: 1.0}
-- |   <+> (Proxy :: _ "health") := {current: 100, max: 100}
-- | ```
-- |
-- | This is equivalent to:
-- | ```purescript
-- | e1 <- spawnEntity
-- | e2 <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e1
-- | e3 <- addComponent (Proxy :: _ "velocity") {x: 1.0, y: 1.0} e2
-- | entity <- addComponent (Proxy :: _ "health") {current: 100, max: 100} e3
-- | ```
-- |
-- | But impossible to accidentally use the wrong intermediate entity.
with :: forall r r' label a.
  IsSymbol label =>
  Lacks label r =>
  Cons label a r r' =>
  State World (Entity r) ->
  ComponentPair label a ->
  State World (Entity r')
with entityAction (ComponentPair proxy value) = do
  entity <- entityAction
  addComponent proxy value entity

infixl 5 with as <+>

-- | Add a component to an entity (pure version for internal use).
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Lacks label r: Proves component doesn't already exist (prevents duplicates)
-- | - Cons label a r r': Adding component produces new row type r'
-- |
-- | Algorithm:
-- | 1. Extract component label as runtime string
-- | 2. Find entity's current archetype
-- | 3. Calculate new archetype ID (add label, sort)
-- | 4. Copy existing components + add new component
-- | 5. Remove entity from old archetype (swap-remove)
-- | 6. Add entity to new archetype
-- | 7. Update entity location tracking
-- |
-- | Returns updated World and Entity with expanded row type.
addComponentPure :: forall r r' label a.
  IsSymbol label =>
  Lacks label r =>
  Cons label a r r' =>
  Proxy label ->
  a ->
  Entity r ->
  World ->
  { world :: World, entity :: Entity r' }
addComponentPure labelProxy componentValue entity world =
  let
    -- Extract label as string
    labelStr = reflectSymbol labelProxy
    entityId = unEntity entity
    idx = entityIndex entityId

    -- Validate entity exists
    isValid = validateEntity entityId world.entities
  in
    if not isValid then
      -- Entity doesn't exist, return unchanged
      -- Note: Type system can't prevent this runtime case
      { world, entity: wrapEntity entityId }
    else
      case Map.lookup idx world.entityLocations of
        Nothing ->
          -- Entity not in any archetype (shouldn't happen)
          { world, entity: wrapEntity entityId }

        Just oldArchId ->
          let
            -- Calculate new archetype ID
            newArchId = addLabelToArchetype labelStr oldArchId

            -- Move entity and store component data
            result = moveEntityWithComponent entityId oldArchId newArchId labelStr (CS.componentToForeign componentValue) world
          in
            { world: result.world, entity: wrapEntity entityId }

-- | Remove a component from an entity (monadic version).
-- |
-- | This is the main API. It works within the State monad over World.
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Cons label a r' r: Proves component exists and can be removed
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   e' <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |   e'' <- removeComponent (Proxy :: _ "position") e'
-- |   pure e''  -- Back to empty entity
-- | ```
removeComponent :: forall r r' label a.
  IsSymbol label =>
  Cons label a r' r =>
  Proxy label ->
  Entity r ->
  State World (Entity r')
removeComponent labelProxy entity = state \world ->
  let result = removeComponentPure labelProxy entity world
  in Tuple result.entity result.world

-- | Remove a component from an entity (pure version for internal use).
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Cons label a r' r: Proves component exists and can be removed
-- |
-- | Algorithm:
-- | 1. Extract component label as runtime string
-- | 2. Find entity's current archetype
-- | 3. Calculate new archetype ID (remove label)
-- | 4. Copy remaining components (excluding removed one)
-- | 5. Remove entity from old archetype
-- | 6. Add entity to new archetype
-- | 7. Update entity location tracking
-- |
-- | Returns updated World and Entity with reduced row type.
removeComponentPure :: forall r r' label a.
  IsSymbol label =>
  Cons label a r' r =>
  Proxy label ->
  Entity r ->
  World ->
  { world :: World, entity :: Entity r' }
removeComponentPure labelProxy entity world =
  let
    -- Extract label as string
    labelStr = reflectSymbol labelProxy
    entityId = unEntity entity
    idx = entityIndex entityId

    -- Validate entity exists
    isValid = validateEntity entityId world.entities
  in
    if not isValid then
      -- Entity doesn't exist, return unchanged
      { world, entity: wrapEntity entityId }
    else
      case Map.lookup idx world.entityLocations of
        Nothing ->
          -- Entity not in any archetype
          { world, entity: wrapEntity entityId }

        Just oldArchId ->
          let
            -- Calculate new archetype ID
            newArchId = removeLabelFromArchetype labelStr oldArchId

            -- Move entity to new archetype (pass label for mask computation)
            result = moveEntityToArchetype entityId oldArchId newArchId labelStr world
          in
            { world: result.world, entity: wrapEntity entityId }

-- | Get a component value from an entity (monadic version).
-- |
-- | This is the main API. It works within the State monad over World.
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Cons label a trash r: Proves component exists in row type
-- |
-- | Returns:
-- | - Just value: If entity valid and component exists
-- | - Nothing: If entity invalid or not found
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   e' <- addComponent (Proxy :: _ "position") {x: 10.0, y: 20.0} e
-- |   maybePos <- getComponent (Proxy :: _ "position") e'
-- |   -- maybePos is Just {x: 10.0, y: 20.0}
-- | ```
getComponent :: forall r label a trash.
  IsSymbol label =>
  Cons label a trash r =>
  Proxy label ->
  Entity r ->
  State World (Maybe a)
getComponent labelProxy entity = do
  world <- get
  pure $ getComponentPure labelProxy entity world

-- | Get a component value from an entity (pure version).
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Cons label a trash r: Proves component exists in row type
-- |
-- | Returns:
-- | - Just value: If entity valid and component exists
-- | - Nothing: If entity invalid or not found
-- |
-- | Note: Type system guarantees component label is valid for this entity type,
-- | but runtime checks still needed for entity validity.
getComponentPure :: forall r label a trash.
  IsSymbol label =>
  Cons label a trash r =>
  Proxy label ->
  Entity r ->
  World ->
  Maybe a
getComponentPure labelProxy entity world =
  let
    entityId = unEntity entity
    idx = entityIndex entityId

    -- Validate entity exists
    isValid = validateEntity entityId world.entities
  in
    if not isValid then
      Nothing
    else
      case Map.lookup idx world.entityLocations of
        Nothing -> Nothing

        Just archId ->
          case Map.lookup archId world.archetypes of
            Nothing -> Nothing

            Just arch ->
              let
                -- Extract label as string
                labelStr = reflectSymbol labelProxy

                -- Find entity's position in archetype using O(log N) map lookup
                maybePos = Map.lookup idx arch.entityPositions
              in
                case maybePos of
                  Nothing -> Nothing
                  Just pos ->
                    -- Access component array for this label
                    case CS.lookup labelStr arch.storage of
                      Nothing -> Nothing
                      Just componentArray ->
                        map CS.componentFromForeign (index componentArray pos)

-- | Check if an entity has a specific component (runtime check).
-- |
-- | Less useful than getComponent since types already track components,
-- | but provided for completeness and runtime inspection.
hasComponent :: forall r label.
  IsSymbol label =>
  Proxy label ->
  Entity r ->
  World ->
  Boolean
hasComponent labelProxy entity world =
  let
    labelStr = reflectSymbol labelProxy
    entityId = unEntity entity
    idx = entityIndex entityId
  in
    case Map.lookup idx world.entityLocations of
      Nothing -> false
      Just archId -> archetypeContains labelStr archId

-- Helper Functions

-- | Move entity with a new component (adding component).
-- |
-- | Extracts existing components, then stores all components in new archetype.
moveEntityWithComponent :: EntityId -> ArchetypeId -> ArchetypeId -> String -> Foreign -> World -> { world :: World }
moveEntityWithComponent entityId oldArchId newArchId label componentValue world =
  let
    idx = entityIndex entityId

    -- Get or create the bit for this component label
    { bit: labelBit, world: worldWithBit } = getOrCreateComponentMask label world

    -- Get the old archetype's mask (or 0 if doesn't exist)
    oldMask = case Map.lookup oldArchId worldWithBit.archetypes of
      Nothing -> 0
      Just arch -> arch.mask

    -- New mask includes the new component
    newMask = maskAddBit oldMask labelBit

    -- Extract existing components before removing using O(log N) position lookup
    existingComponents = case Map.lookup oldArchId worldWithBit.archetypes of
      Nothing -> CS.empty  -- No old archetype, no components
      Just arch ->
        case Map.lookup idx arch.entityPositions of
          Nothing -> CS.empty  -- Entity not found in old archetype
          Just entityIdx -> extractAllComponentsForEntity entityIdx arch.storage

    -- Remove from old archetype
    world1 = removeFromArchetype entityId oldArchId worldWithBit

    -- Add to new archetype with ALL components (existing + new) and new mask
    world2 = addToArchetypeWithComponent entityId newArchId newMask existingComponents label componentValue world1

    -- Update entity location
    updatedLocations = Map.insert idx newArchId world2.entityLocations
  in
    { world: world2 { entityLocations = updatedLocations } }

-- | Move entity from one archetype to another (for removal).
-- |
-- | Extracts and preserves remaining components after removal.
-- | Used by removeComponent to migrate entity to archetype without removed component.
moveEntityToArchetype :: EntityId -> ArchetypeId -> ArchetypeId -> String -> World -> { world :: World }
moveEntityToArchetype entityId oldArchId newArchId removedLabel world =
  let
    idx = entityIndex entityId

    -- Get the bit for the removed label (should already exist)
    removedBit = case Map.lookup removedLabel world.componentRegistry.labelToBit of
      Just bit -> bit
      Nothing -> 0  -- Shouldn't happen, but fallback

    -- Get the old archetype's mask and remove the bit
    oldMask = case Map.lookup oldArchId world.archetypes of
      Nothing -> 0
      Just arch -> arch.mask
    newMask = maskRemoveBit oldMask removedBit

    -- Extract existing components before removing using O(log N) position lookup
    existingComponents = case Map.lookup oldArchId world.archetypes of
      Nothing -> CS.empty  -- No old archetype, no components
      Just arch ->
        case Map.lookup idx arch.entityPositions of
          Nothing -> CS.empty  -- Entity not found in old archetype
          Just entityIdx -> extractAllComponentsForEntity entityIdx arch.storage

    -- Filter to only components that exist in new archetype ID
    newArchLabels = parseArchetypeId newArchId
    filteredComponents = CS.filterKeys (\label ->
      elem label newArchLabels
    ) existingComponents

    -- Remove from old archetype
    world1 = removeFromArchetype entityId oldArchId world

    -- Add to new archetype with filtered components and new mask
    world2 = addToArchetypeWithAllComponents entityId newArchId newMask filteredComponents world1

    -- Update entity location
    updatedLocations = Map.insert idx newArchId world2.entityLocations
  in
    { world: world2 { entityLocations = updatedLocations } }

-- | Add entity to archetype with multiple components (used by removeComponent).
addToArchetypeWithAllComponents :: EntityId -> ArchetypeId -> ComponentMask -> ComponentStorage -> World -> World
addToArchetypeWithAllComponents entityId archId newMask allComponents world =
  let
    -- Get or create archetype
    arch = case Map.lookup archId world.archetypes of
      Just a -> a
      Nothing -> { entities: [], entityPositions: Map.empty, mask: newMask, storage: CS.empty }

    -- For each component, append to archetype's component arrays
    updatedStorage = CS.fold (\acc label singletonArray ->
      let existingArray = case CS.lookup label arch.storage of
            Just arr -> arr
            Nothing -> CS.emptyArray
          -- singletonArray is [componentValue] from extractAllComponentsForEntity
          updatedArray = existingArray <> singletonArray
      in CS.insert label updatedArray acc
    ) arch.storage allComponents

    -- Add entity with position tracking (mask is set on creation or preserved)
    newPosition = length arch.entities
    updatedArch =
      { entities: arch.entities <> [entityId]
      , entityPositions: Map.insert (entityIndex entityId) newPosition arch.entityPositions
      , mask: arch.mask  -- Preserve existing mask
      , storage: updatedStorage
      }
    updatedArchetypes = Map.insert archId updatedArch world.archetypes
  in
    world { archetypes = updatedArchetypes }

-- | Remove entity from archetype (swap-remove with O(log N) lookup).
removeFromArchetype :: EntityId -> ArchetypeId -> World -> World
removeFromArchetype entityId archId world =
  case Map.lookup archId world.archetypes of
    Nothing -> world  -- Archetype doesn't exist

    Just arch ->
      let
        idx = entityIndex entityId
        -- Find entity position using O(log N) map lookup
        maybePos = Map.lookup idx arch.entityPositions
      in
        case maybePos of
          Nothing -> world  -- Entity not in archetype
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

              -- Update archetype (preserve mask - archetype signature doesn't change)
              updatedArch =
                { entities: result.entities
                , entityPositions: result.positions
                , mask: arch.mask  -- Mask stays the same
                , storage: updatedStorage
                }
              updatedArchetypes = Map.insert archId updatedArch world.archetypes
            in
              world { archetypes = updatedArchetypes }

-- | Extract all component values for an entity at a specific index.
-- |
-- | Returns a ComponentStorage where each label maps to a single component value
-- | (wrapped in a singleton array for consistency with archetype structure).
-- |
-- | Example: index=1 → storage with [position1], [velocity1] (arrays of size 1)
extractAllComponentsForEntity :: Int -> ComponentStorage -> ComponentStorage
extractAllComponentsForEntity idx storage =
  CS.fold (\acc label componentArray ->
    case CS.arrayIndex idx componentArray of
      Just componentValue -> CS.insert label (CS.arrayFromSingleton componentValue) acc
      Nothing -> acc  -- Should never happen in correct usage
  ) CS.empty storage

-- Helper for array range
range :: Int -> Int -> Array Int
range start end =
  if start > end then []
  else start : range (start + 1) end

infixl 8 range as ..

-- | Add entity to archetype with components (both existing and new).
-- |
-- | mask: Precomputed bitmask for the new archetype
-- | existingComponents: ComponentStorage of component values to preserve (from old archetype)
-- | label: new component label to add
-- | componentValue: new component value to add
addToArchetypeWithComponent :: EntityId -> ArchetypeId -> ComponentMask -> ComponentStorage -> String -> Foreign -> World -> World
addToArchetypeWithComponent entityId archId newMask existingComponents newLabel newComponentValue world =
  let
    -- Get or create archetype with component storage
    arch = case Map.lookup archId world.archetypes of
      Just a -> a
      Nothing -> { entities: [], entityPositions: Map.empty, mask: newMask, storage: CS.empty }

    -- Merge existing components + new component (wrap single value in array)
    allComponents = CS.insert newLabel (CS.arrayFromSingleton newComponentValue) existingComponents

    -- For each component in allComponents, append to the archetype's component arrays
    updatedStorage = CS.fold (\acc label singletonArray ->
      let existingArray = case CS.lookup label arch.storage of
            Just arr -> arr
            Nothing -> CS.emptyArray
          -- singletonArray is [newComponentValue] from the insert above
          updatedArray = existingArray <> singletonArray
      in CS.insert label updatedArray acc
    ) arch.storage allComponents

    -- Add entity with position tracking (mask is set on creation or preserved)
    newPosition = length arch.entities
    updatedArch =
      { entities: arch.entities <> [entityId]
      , entityPositions: Map.insert (entityIndex entityId) newPosition arch.entityPositions
      , mask: arch.mask  -- Preserve existing mask (already set correctly)
      , storage: updatedStorage
      }
    updatedArchetypes = Map.insert archId updatedArch world.archetypes
  in
    world { archetypes = updatedArchetypes }

-- | Parse archetype ID into component labels.
-- |
-- | "Position,Velocity" -> ["Position", "Velocity"]
-- | "" -> []
parseArchetypeId :: ArchetypeId -> Array String
parseArchetypeId "" = []
parseArchetypeId archId =
  map trim $ split (Pattern ",") archId

-- | Build archetype ID from component labels (sorted).
-- |
-- | ["Velocity", "Position"] -> "Position,Velocity"
-- | [] -> ""
buildArchetypeId :: Array String -> ArchetypeId
buildArchetypeId [] = ""
buildArchetypeId labels =
  joinWith "," (sort labels)

-- | Add label to archetype ID (maintains sorted order).
-- |
-- | "Velocity" + "Position" -> "Position,Velocity"
addLabelToArchetype :: String -> ArchetypeId -> ArchetypeId
addLabelToArchetype label archId =
  let
    labels = parseArchetypeId archId
    newLabels = label : labels
  in
    buildArchetypeId newLabels

-- | Remove label from archetype ID.
-- |
-- | "Position,Velocity" - "Position" -> "Velocity"
removeLabelFromArchetype :: String -> ArchetypeId -> ArchetypeId
removeLabelFromArchetype label archId =
  let
    labels = parseArchetypeId archId
    newLabels = filter (_ /= label) labels
  in
    buildArchetypeId newLabels

-- | Check if archetype contains a component label.
-- |
-- | "Position" in "Position,Velocity" -> true
-- | "Health" in "Position,Velocity" -> false
archetypeContains :: String -> ArchetypeId -> Boolean
archetypeContains label archId =
  let labels = parseArchetypeId archId
  in case findIndex (_ == label) labels of
       Just _ -> true
       Nothing -> false
