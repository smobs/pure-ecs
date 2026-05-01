module Test.ECS.PipelineSpec (pipelineSpec) where

import Prelude

import Control.Monad.State (execState, state)
import Data.Array (length)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import ECS.Component ((<+>), (:=))
import ECS.Pipeline (Pipeline, PCons, PNil, named, pipeline, runPipeline, (>->))
import ECS.Query (query, runQuery)
import ECS.System (System, modifyComponent_, queryFor, runSystem)
import ECS.World (emptyWorld, spawnEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health   = { current :: Int, max :: Int }
type Tick     = { trace :: String }

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

  it "runs steps in left-to-right order" do
    let world0 = execState (
          void $ spawnEntity
            <+> (Proxy :: _ "tick") := { trace: "" }
        ) emptyWorld

        appendTrace :: String -> System (tick :: Tick) (tick :: Tick) Unit
        appendTrace s = do
          results <- queryFor @(tick :: Tick)
          for_ results \r ->
            modifyComponent_ (Proxy :: _ "tick") (\t -> { trace: t.trace <> s }) r.entity

        p :: Pipeline "order"
              (PCons "a" (tick :: Tick) (tick :: Tick)
                (PCons "b" (tick :: Tick) (tick :: Tick)
                  (PCons "c" (tick :: Tick) (tick :: Tick) PNil)))
              Unit
        p = pipeline @"order" (named @"a" (appendTrace "A"))
          >-> named @"b" (appendTrace "B")
          >-> named @"c" (appendTrace "C")

        { world: world1 } = runPipeline p world0
        traces = runQuery (query (Proxy :: _ (tick :: Tick))) world1
    case traces of
      [r] -> r.components.tick.trace `shouldEqual` "ABC"
      _ -> fail $ "expected exactly one tick entity, got " <> show (length traces)

  it "runPipeline of a 1-step pipeline equals runSystem of that step" do
    let s :: System () () Int
        s = pure 42

        viaPipeline = runPipeline (pipeline @"solo" (named @"only" s)) emptyWorld
        viaSystem   = runSystem s emptyWorld
    viaPipeline.result `shouldEqual` viaSystem.result
