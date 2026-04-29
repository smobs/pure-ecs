module Bench.Main where

import Prelude

import Bench.ECS.Bench (Scenario, spawn1k, queryAll1k, fullTick1k)
import Data.Array (range)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (for_)
import Data.Int (round, toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Ref as Ref

scenarios :: Array Scenario
scenarios = [ spawn1k, queryAll1k, fullTick1k ]

iterations :: Int
iterations = 5

-- | Force the scenario by deriving an Int from the resulting World and
-- | writing it to a Ref. The Ref is later read and printed, so the
-- | optimiser cannot dead-code-eliminate the work.
bench :: Scenario -> Effect Unit
bench scenario = do
  sinkRef <- Ref.new 0
  -- Warm-up
  let warm0 = scenario.run scenario.build
  Ref.write warm0.structuralVersion sinkRef

  totalRef <- Ref.new 0.0
  for_ (range 0 (iterations - 1)) \_ -> do
    t0 <- now
    let result = scenario.run scenario.build
    Ref.write result.structuralVersion sinkRef
    t1 <- now
    let Milliseconds m0 = unInstant t0
    let Milliseconds m1 = unInstant t1
    Ref.modify_ (_ + (m1 - m0)) totalRef

  total <- Ref.read totalRef
  sink <- Ref.read sinkRef
  let avg = total / toNumber iterations
  log $ scenario.name
        <> ": " <> show (round avg)
        <> " ms (avg of " <> show iterations
        <> ", sink=" <> show sink <> ")"

main :: Effect Unit
main = for_ scenarios bench
