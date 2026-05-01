module Test.ECS.DocsSpec (docsSpec) where

import Prelude

import ECS.Docs (collectSteps)
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
