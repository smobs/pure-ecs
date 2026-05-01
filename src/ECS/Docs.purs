-- | ECS Pipeline Documentation Generation
-- |
-- | Walks the type-level step list of a Pipeline value and emits markdown
-- | documenting the systems, their reads/writes, the execution order, and
-- | the inter-system data flow.
-- |
-- | The same Pipeline value is consumed by ECS.Pipeline.runPipeline at game
-- | runtime. There is no separate manifest; documentation cannot drift.
module ECS.Docs
  ( StepDoc
  , class DocumentSteps
  , collectSteps
  , documentPipeline
  ) where

import Prelude

import Data.Set as Set
import ECS.Pipeline (Pipeline, PCons, PNil, PipelineList)
import ECS.Query (class ExtractLabels, extractLabels)
import Prim.RowList (class RowToList)
import Type.Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

type StepDoc =
  { name   :: String
  , reads  :: Array String
  , writes :: Array String
  }

class DocumentSteps (steps :: PipelineList) where
  collectSteps :: Proxy steps -> Array StepDoc

instance documentStepsNil :: DocumentSteps PNil where
  collectSteps _ = []

instance documentStepsCons ::
  ( IsSymbol name
  , RowToList r rl
  , RowToList w wl
  , ExtractLabels rl
  , ExtractLabels wl
  , DocumentSteps rest
  ) => DocumentSteps (PCons name r w rest) where
  collectSteps _ =
    [ { name:   reflectSymbol (Proxy :: Proxy name)
      , reads:  (Set.toUnfoldable (extractLabels (Proxy :: Proxy rl)) :: Array String)
      , writes: (Set.toUnfoldable (extractLabels (Proxy :: Proxy wl)) :: Array String)
      }
    ] <> collectSteps (Proxy :: Proxy rest)

-- | Generate markdown documentation for a pipeline. Placeholder body for
-- | Task 2a; the real renderer lands in Task 2b.
documentPipeline
  :: forall pname steps a
   . IsSymbol pname
  => DocumentSteps steps
  => Pipeline pname steps a
  -> String
documentPipeline _ =
  let pname = reflectSymbol (Proxy :: Proxy pname)
      steps = collectSteps (Proxy :: Proxy steps)
  in "# Pipeline: " <> pname <> "\n\n(steps: " <> show (map _.name steps) <> ")"
