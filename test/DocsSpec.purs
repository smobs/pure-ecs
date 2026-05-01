module Test.ECS.DocsSpec (docsSpec) where

import Prelude

import Data.String (joinWith)
import ECS.Docs (collectSteps, renderMarkdown)
import ECS.Pipeline (PCons, PNil)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health   = { current :: Int, max :: Int }

type SampleSteps =
  PCons "physics" (position :: Position, velocity :: Velocity) (position :: Position)
    ( PCons "damage" (health :: Health) (health :: Health)
        ( PCons "cleanup" (health :: Health) ()
            PNil ) )

docsSpec :: Spec Unit
docsSpec = describe "ECS.Docs" do
  describe "collectSteps" do
    it "returns step records in execution order" do
      let docs = collectSteps (Proxy :: Proxy SampleSteps)
      map _.name docs `shouldEqual` ["physics", "damage", "cleanup"]

    it "extracts reads as alphabetically-sorted label arrays" do
      let docs = collectSteps (Proxy :: Proxy SampleSteps)
      map _.reads docs `shouldEqual`
        [ ["position", "velocity"]
        , ["health"]
        , ["health"]
        ]

    it "extracts writes (including empty)" do
      let docs = collectSteps (Proxy :: Proxy SampleSteps)
      map _.writes docs `shouldEqual`
        [ ["position"]
        , ["health"]
        , []
        ]

  describe "renderMarkdown" do
    it "produces the expected markdown for a 3-step fixture" do
      let fixture =
            [ { name: "physics", reads: ["position", "velocity"], writes: ["position"] }
            , { name: "damage",  reads: ["damage", "health"],     writes: ["health"] }
            , { name: "cleanup", reads: ["health"],               writes: [] }
            ]
          expected = joinWith "\n"
            [ "# Pipeline: gameTick"
            , ""
            , "## Execution order"
            , "1. physics"
            , "2. damage"
            , "3. cleanup"
            , ""
            , "## Data flow"
            , "```mermaid"
            , "flowchart LR"
            , "    damage -->|health| cleanup"
            , "```"
            , ""
            , "## Systems"
            , "### physics"
            , "- **Reads:** `position`, `velocity`"
            , "- **Writes:** `position`"
            , ""
            , "### damage"
            , "- **Reads:** `damage`, `health`"
            , "- **Writes:** `health`"
            , ""
            , "### cleanup"
            , "- **Reads:** `health`"
            , "- **Writes:** *(none)*"
            , ""
            , "## Components touched"
            , "| Component | Read by | Written by |"
            , "|-----------|---------|------------|"
            , "| damage | damage | — |"
            , "| health | damage, cleanup | damage |"
            , "| position | physics | physics |"
            , "| velocity | physics | — |"
            , ""
            ]
      renderMarkdown "gameTick" fixture `shouldEqual` expected
