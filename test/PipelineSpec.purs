module Test.ECS.PipelineSpec (pipelineSpec) where

import Prelude

import Control.Monad.State (state)
import Data.Tuple (Tuple(..))
import ECS.Pipeline (Pipeline, PCons, PNil, named, pipeline, runPipeline, (>->))
import ECS.System (System)
import ECS.World (emptyWorld)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health   = { current :: Int, max :: Int }

-- Three trivial systems that we'll compose into a pipeline.
sysA :: System (position :: Position, velocity :: Velocity) (position :: Position) Int
sysA = state \w -> Tuple 1 w

sysB :: System (health :: Health) (health :: Health) Int
sysB = state \w -> Tuple 2 w

sysC :: System (health :: Health) () Int
sysC = state \w -> Tuple 3 w

-- The pipeline value with its full inferred type written out, to lock the
-- type-level shape of the spine.
samplePipeline
  :: Pipeline "gameTick"
       (PCons "a" (position :: Position, velocity :: Velocity) (position :: Position)
         (PCons "b" (health :: Health) (health :: Health)
           (PCons "c" (health :: Health) () PNil)))
       Int
samplePipeline =
       pipeline @"gameTick" (named @"a" sysA)
  >-> named @"b" sysB
  >-> named @"c" sysC

pipelineSpec :: Spec Unit
pipelineSpec = describe "ECS.Pipeline (skeleton)" do
  it "constructs a 3-step pipeline and returns the last step's result" do
    let { result } = runPipeline samplePipeline emptyWorld
    result `shouldEqual` 3
