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
  , renderMarkdown
  ) where

import Prelude

import Data.Array (filter, length, mapWithIndex, nub, sort)
import Data.Foldable (foldMap)
import Data.Set as Set
import Data.String (joinWith)
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

-- | Generate markdown documentation for a pipeline.
documentPipeline
  :: forall pname steps a
   . IsSymbol pname
  => DocumentSteps steps
  => Pipeline pname steps a
  -> String
documentPipeline _ =
  renderMarkdown (reflectSymbol (Proxy :: Proxy pname))
                 (collectSteps (Proxy :: Proxy steps))

-- | Pure data → markdown. Exposed so tests can exercise the renderer
-- | without going through the type-level machinery.
renderMarkdown :: String -> Array StepDoc -> String
renderMarkdown pname steps =
  joinWith "\n"
    [ "# Pipeline: " <> pname
    , ""
    , "## Execution order"
    , renderExecutionOrder steps
    , ""
    , "## Data flow"
    , renderDataFlow steps
    , ""
    , "## Systems"
    , renderSystems steps
    , "## Components touched"
    , renderComponentsTable steps
    ]

renderExecutionOrder :: Array StepDoc -> String
renderExecutionOrder steps =
  joinWith "\n" $
    mapWithIndex (\i s -> show (i + 1) <> ". " <> s.name) steps

renderDataFlow :: Array StepDoc -> String
renderDataFlow steps =
  let edges = dataFlowEdges steps
  in if length edges == 0
       then "```mermaid\nflowchart LR\n    %% no inter-system data flow\n```"
       else "```mermaid\nflowchart LR\n"
         <> foldMap (\e -> "    " <> e.from <> " -->|" <> e.component
                              <> "| " <> e.to <> "\n") edges
         <> "```"

renderSystems :: Array StepDoc -> String
renderSystems steps =
  joinWith "\n" $ map renderOne steps
  where
    renderOne s =
      "### " <> s.name <> "\n"
        <> "- **Reads:** " <> formatLabels s.reads <> "\n"
        <> "- **Writes:** " <> formatLabels s.writes <> "\n"
    formatLabels [] = "*(none)*"
    formatLabels xs = joinWith ", " (map (\l -> "`" <> l <> "`") xs)

renderComponentsTable :: Array StepDoc -> String
renderComponentsTable steps =
  let allComponents = sort $ nub $ steps >>= \s -> s.reads <> s.writes
  in if length allComponents == 0
       then "*(no components)*"
       else
         "| Component | Read by | Written by |\n"
         <> "|-----------|---------|------------|\n"
         <> foldMap (renderRow steps) allComponents
  where
    renderRow steps' c =
      let readers = map _.name $ filter (\s -> elem' c s.reads)  steps'
          writers = map _.name $ filter (\s -> elem' c s.writes) steps'
      in "| " <> c <> " | " <> formatList readers <> " | " <> formatList writers <> " |\n"
    formatList [] = "—"
    formatList xs = joinWith ", " xs
    elem' x = (_ /= 0) <<< length <<< filter (_ == x)

-- | One directed data-flow edge: writer → reader, labelled with component.
type Edge = { from :: String, to :: String, component :: String }

-- | All (writer, reader, component) triples where the writer step appears
-- | strictly before the reader in execution order.
dataFlowEdges :: Array StepDoc -> Array Edge
dataFlowEdges steps =
  let indexed = mapWithIndex (\i s -> { i, s }) steps
  in indexed >>= \w ->
     w.s.writes >>= \c ->
     filter (\r -> r.i > w.i && elem' c r.s.reads) indexed >>= \r ->
       [ { from: w.s.name, to: r.s.name, component: c } ]
  where
    elem' x = (_ /= 0) <<< length <<< filter (_ == x)
