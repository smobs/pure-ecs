-- | ECS Monadic API: State Monad Wrapper for World Operations
-- |
-- | This module provides a monadic interface for ECS operations, eliminating
-- | the need for manual world threading. All operations work within the WorldM
-- | state monad, which automatically manages the World state.
-- |
-- | Key concepts:
-- | - WorldM: State monad over World for clean composition
-- | - Automatic world threading via do-notation
-- | - Same type safety as direct API
-- | - Zero runtime overhead (State monad is a newtype)
-- |
-- | Example usage:
-- | ```purescript
-- | setupWorld :: WorldM (Entity (position :: Position, velocity :: Velocity))
-- | setupWorld = do
-- |   e <- spawnEntity
-- |   e' <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |   e'' <- addComponent (Proxy :: _ "velocity") {x: 1.0, y: 1.0} e'
-- |   pure e''
-- |
-- | let world = execWorld setupWorld emptyWorld
-- | ```
module ECS.WorldM
  ( WorldM
  , runWorld
  , execWorld
  , evalWorld
  , getWorld
  , putWorld
  , modifyWorld
  ) where

import Prelude

import Control.Monad.State (State, runState, execState, evalState, get, put, modify)
import Data.Tuple (Tuple)
import ECS.World (World)

-- | WorldM is a State monad over World.
-- |
-- | This is the monad that all ECS operations work within. It automatically
-- | threads the World state through all operations, eliminating manual chaining.
-- |
-- | Type parameter:
-- | - a: The result type of the computation
-- |
-- | Example:
-- | - WorldM (Entity ()): Computation that produces an empty entity
-- | - WorldM Int: Computation that produces a number (e.g., entity count)
-- | - WorldM Unit: Computation with no meaningful result
type WorldM a = State World a

-- | Run a WorldM computation, returning both the result and final world.
-- |
-- | Example:
-- | ```purescript
-- | let Tuple entity finalWorld = runWorld (do
-- |       e <- spawnEntity
-- |       addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |     ) emptyWorld
-- | ```
runWorld :: forall a. WorldM a -> World -> Tuple a World
runWorld = runState

-- | Execute a WorldM computation, returning only the final world.
-- |
-- | This is the most common way to use WorldM for world setup.
-- |
-- | Example:
-- | ```purescript
-- | let world = execWorld (do
-- |       e <- spawnEntity
-- |       addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
-- |     ) emptyWorld
-- | ```
execWorld :: forall a. WorldM a -> World -> World
execWorld = execState

-- | Evaluate a WorldM computation, returning only the result.
-- |
-- | Useful when you only care about the computed value, not the final world.
-- |
-- | Example:
-- | ```purescript
-- | let entityCount = evalWorld (do
-- |       e1 <- spawnEntity
-- |       e2 <- spawnEntity
-- |       pure 2
-- |     ) emptyWorld  -- Returns 2
-- | ```
evalWorld :: forall a. WorldM a -> World -> a
evalWorld = evalState

-- | Get the current world state.
-- |
-- | Useful for inspecting the world within a WorldM computation.
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   e <- spawnEntity
-- |   world <- getWorld
-- |   -- inspect world...
-- |   pure e
-- | ```
getWorld :: WorldM World
getWorld = get

-- | Set the world state.
-- |
-- | Replaces the current world with a new one. Use with caution.
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   putWorld emptyWorld  -- Reset to empty world
-- | ```
putWorld :: World -> WorldM Unit
putWorld = put

-- | Modify the world state with a function.
-- |
-- | Example:
-- | ```purescript
-- | do
-- |   modifyWorld \w -> -- some transformation
-- | ```
modifyWorld :: (World -> World) -> WorldM Unit
modifyWorld f = modify f
