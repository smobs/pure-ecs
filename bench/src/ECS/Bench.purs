module Bench.ECS.Bench
  ( Scenario
  , spawn1k
  , queryAll1k
  , fullTick1k
  ) where

import Prelude

import Control.Monad.State (execState)
import Data.Array (range)
import Data.Foldable (for_)
import Data.Int (toNumber)
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, modifyComponent_, queryFor)
import ECS.World (emptyWorld, spawnEntity, World)
import Type.Proxy (Proxy(..))

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health   = { current :: Int, max :: Int }

type Scenario =
  { name  :: String
  , build :: World
  , run   :: World -> World
  }

buildWorld :: Int -> World
buildWorld n = execState
  (for_ (range 0 (n - 1)) \i ->
      spawnEntity
        <+> (Proxy :: _ "position") := { x: toNumber i, y: toNumber i }
        <+> (Proxy :: _ "velocity") := { x: 1.0, y: 1.0 }
        <+> (Proxy :: _ "health")   := { current: 100, max: 100 })
  emptyWorld

movementSystem :: System ( position :: Position, velocity :: Velocity )
                          ( position :: Position )
                          Unit
movementSystem = do
  rs <- queryFor @( position :: Position, velocity :: Velocity )
  for_ rs \r ->
    modifyComponent_ (Proxy :: _ "position")
      (\p -> { x: p.x + r.components.velocity.x
             , y: p.y + r.components.velocity.y })
      r.entity

spawn1k :: Scenario
spawn1k =
  { name: "spawn 1000 entities (3 components each)"
  , build: emptyWorld
  , run: \_ -> buildWorld 1000
  }

queryAll1k :: Scenario
queryAll1k =
  { name: "query+update (pos, vel) over 1000 entities"
  , build: buildWorld 1000
  , run: \w -> (runSystem movementSystem w).world
  }

fullTick1k :: Scenario
fullTick1k =
  { name: "10 movement ticks over 1000 entities"
  , build: buildWorld 1000
  , run: \w -> tickN 10 w
  }
  where
    tickN 0 w = w
    tickN k w = tickN (k - 1) (runSystem movementSystem w).world
