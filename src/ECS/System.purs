-- | ECS Phase 5: Systems with Access Tracking
-- |
-- | This module implements pure functional systems with phantom type access tracking.
-- | Systems are World → World transformations that declare which components they read
-- | and write at compile-time, enabling automatic conflict detection and validation.
-- |
-- | Key concepts:
-- | - System: Pure function wrapped with access metadata
-- | - Read set: Components the system reads (phantom type)
-- | - Write set: Components the system writes (phantom type)
-- | - Access validation: Compile-time constraints ensure safety
-- | - Composition: Systems chain sequentially with merged access
module ECS.System
  ( System
  , runSystem
  , query
  , updateComponent
  ) where

import Control.Monad.State (State, state, runState)
import Data.Tuple (Tuple(..))
import ECS.Component (addComponentPure, removeComponentPure)
import ECS.Query (Query, QueryResult, class ExtractLabels, class ReadComponents)
import ECS.Query (runQuery) as Q
import ECS.World (World, Entity)
import Prim.Row (class Union, class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol)

-- | System is a State monad over World with phantom type access tracking.
-- |
-- | Type parameters:
-- | - reads: Row of components this system reads
-- | - writes: Row of components this system writes
-- | - a: Return type (usually Unit or analysis results)
-- |
-- | A component can appear in both reads and writes (read-modify-write pattern).
-- | Phantom types (reads, writes) exist only at compile-time for access tracking.
-- |
-- | Example:
-- | ```purescript
-- | movementSystem :: Number -> System (position :: Position, velocity :: Velocity)
-- |                                    (position :: Position)
-- |                                    Unit
-- | movementSystem dt = do
-- |   results <- query (query {position: ..., velocity: ...})
-- |   for_ results \r -> do
-- |     when (shouldMove r) do
-- |       updateComponent (Proxy :: _ "position") newPos r.entity
-- | ```
type System (reads :: Row Type) (writes :: Row Type) a = State World a

-- | Run a system on a world.
-- |
-- | Returns both the updated world and the system's result value.
-- |
-- | Example:
-- | ```purescript
-- | let { world: world', result } = runSystem mySystem world
-- | ```
runSystem :: forall reads writes a.
  System reads writes a ->
  World ->
  { world :: World, result :: a }
runSystem system world =
  let (Tuple result world') = runState system world
  in { world: world', result }

-- | Query within a system with access validation.
-- |
-- | Type constraint ensures queried components are in the read set:
-- | - Union required extra reads: Proves required ⊆ reads
-- |
-- | Example:
-- | ```purescript
-- | mySystem = do
-- |   results <- query (query (Proxy :: _ (position :: Position, velocity :: Velocity)))
-- |   -- process results...
-- | ```
query :: forall required excluded reads writes extra rl.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Union required extra reads =>
  Query required excluded ->
  System reads writes (Array (QueryResult required))
query q = state \world ->
  Tuple (Q.runQuery q world) world

-- | Update a component within a system.
-- |
-- | This is implemented as remove-then-add to handle the type system properly.
-- | The component must exist (Cons proves it) and must be in the write set.
-- | Works cleanly with do-notation - no nested runSystem calls needed!
-- |
-- | Type constraints:
-- | - IsSymbol label: Label is a compile-time string
-- | - Cons label a r' r: Proves component exists in entity
-- | - Cons label a trash writes: Proves component is in write set
-- | - Lacks label r': After removal, component doesn't exist
-- |
-- | Example:
-- | ```purescript
-- | mySystem = do
-- |   entity' <- updateComponent (Proxy :: _ "position") {x: 10.0, y: 10.0} entity
-- |   updateComponent (Proxy :: _ "velocity") {x: 0.0, y: 0.0} entity'
-- | ```
updateComponent :: forall label a r r' writes trash.
  IsSymbol label =>
  Cons label a r' r =>           -- Component exists in entity
  Cons label a trash writes =>   -- Component is in write set
  Lacks label r' =>              -- After removal, component doesn't exist
  Proxy label ->
  a ->
  Entity r ->
  System r' writes (Entity r)
updateComponent label newValue entity = state \world ->
  let -- Remove component (entity type: r -> r')
      {world: world1, entity: entity'} = removeComponentPure label entity world
      -- Add component back with new value (entity type: r' -> r)
      {world: world2, entity: entity''} = addComponentPure label newValue entity' world1
  in Tuple entity'' world2
