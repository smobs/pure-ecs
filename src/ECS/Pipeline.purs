-- | ECS Pipeline: Named-System Composition with Doc-Tracking
-- |
-- | A Pipeline is a sequence of NamedSystem values whose ordering, names,
-- | reads, and writes are tracked at the type level. The same Pipeline value
-- | is consumed by `runPipeline` (to execute the game loop) and by
-- | `ECS.Docs.documentPipeline` (to emit markdown docs). The runtime artifact
-- | and the doc source are the same value; they cannot drift.
module ECS.Pipeline
  ( NamedSystem
  , Pipeline
  , PipelineList
  , PNil
  , PCons
  , class SnocStep
  , named
  , pipeline
  , appendStep
  , (>->)
  , runPipeline
  , unsafeRunNamed
  ) where

import Control.Monad.State (runState)
import Data.Tuple (Tuple(..))
import ECS.System (System)
import ECS.World (World)
import Type.Data.Symbol (class IsSymbol)

-- | A System tagged with a compile-time name. The phantom `name` is what
-- | makes documentation tracking possible.
newtype NamedSystem (name :: Symbol) (reads :: Row Type) (writes :: Row Type) a
  = NamedSystem (System reads writes a)

-- | A composed pipeline.
-- |   pname  : the pipeline's own name (for `# Pipeline: <pname>` headers)
-- |   steps  : type-level cons list of step descriptors
-- |   a      : final return type (the result of the last step)
-- |
-- | Constructed only via `pipeline` and `>->`; there is no public constructor
-- | for the empty pipeline, so every Pipeline has at least one step.
data Pipeline (pname :: Symbol) (steps :: PipelineList) a
  = Pipeline (World -> { world :: World, result :: a })

-- | Type-level list of pipeline steps. Each PCons carries the step name,
-- | reads row, and writes row.
foreign import data PipelineList :: Type
foreign import data PNil  :: PipelineList
foreign import data PCons :: Symbol -> Row Type -> Row Type -> PipelineList -> PipelineList

-- | Snoc-append at the type level. `SnocStep xs n r w ys` proves that
-- | appending `(n, r, w)` to the end of `xs` yields `ys`.
class SnocStep
  (xs :: PipelineList) (n :: Symbol) (r :: Row Type) (w :: Row Type)
  (ys :: PipelineList)
  | xs n r w -> ys

instance snocStepNil ::
  SnocStep PNil n r w (PCons n r w PNil)

instance snocStepCons ::
  SnocStep tail n r w tail'
  => SnocStep (PCons n0 r0 w0 tail) n r w (PCons n0 r0 w0 tail')

-- | Wrap a System with a name. Visible type application picks the name:
-- | `named @"physics" $ do ...`
named
  :: forall @name r w a
   . IsSymbol name
  => System r w a
  -> NamedSystem name r w a
named = NamedSystem

-- | Internal: unwrap a NamedSystem so we can run it. Exported for use by
-- | `runPipeline` and (later) by ECS.Docs reflection helpers; not for
-- | end-user code.
unsafeRunNamed :: forall name r w a. NamedSystem name r w a -> System r w a
unsafeRunNamed (NamedSystem s) = s

-- | Start a pipeline from its first step.
pipeline
  :: forall @pname name r w a
   . IsSymbol pname
  => NamedSystem name r w a
  -> Pipeline pname (PCons name r w PNil) a
pipeline (NamedSystem sys) = Pipeline \world ->
  let Tuple result world' = runState sys world
  in { world: world', result }

-- | Append a step to a pipeline. The new step's result becomes the
-- | pipeline's result.
appendStep
  :: forall pname steps name r w a b ys
   . SnocStep steps name r w ys
  => Pipeline pname steps a
  -> NamedSystem name r w b
  -> Pipeline pname ys b
appendStep (Pipeline runPrev) (NamedSystem next) = Pipeline \world ->
  let prev = runPrev world
      Tuple result world' = runState next prev.world
  in { world: world', result }

infixl 1 appendStep as >->

-- | Execute a pipeline against a world, returning the new world and the
-- | result of the final step.
runPipeline
  :: forall pname steps a
   . Pipeline pname steps a
  -> World
  -> { world :: World, result :: a }
runPipeline (Pipeline f) = f
