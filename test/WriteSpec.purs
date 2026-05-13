module Test.ECS.Docs.WriteSpec (writeSpec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String as String
import ECS.Docs.Write (upsertPipelineSection)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

payload :: { pname :: String, docPath :: String, stepNames :: Array String }
payload =
  { pname:     "gameTick"
  , docPath:   "docs/example-pipeline.md"
  , stepNames: ["physics", "damage", "cleanup"]
  }

beginMarker :: String
beginMarker = "<!-- pure-ecs:gameTick:begin -->"

endMarker :: String
endMarker = "<!-- pure-ecs:gameTick:end -->"

writeSpec :: Spec Unit
writeSpec = describe "ECS.Docs.Write.upsertPipelineSection" do
  it "appends a block to an empty file with no leading separator" do
    let out = upsertPipelineSection payload ""
    String.take (String.length beginMarker) out `shouldEqual` beginMarker
    out `shouldSatisfy` contains (Pattern endMarker)
    out `shouldSatisfy` contains (Pattern "**Execution order:** physics → damage → cleanup")

  it "appends a block to a file with no markers, preserving prior content" do
    let prior = "# Project\n\nSome guidance.\n"
        out   = upsertPipelineSection payload prior
    String.take (String.length prior) out `shouldEqual` prior
    out `shouldSatisfy` contains (Pattern beginMarker)
    out `shouldSatisfy` contains (Pattern endMarker)

  it "leaves a blank line between prior content and the marker (file ends in newline)" do
    let prior = "hello\n"
        out   = upsertPipelineSection payload prior
    -- Expect "hello\n" + "\n<!-- ..." → byte at prior-end is "\n" (the
    -- separating newline), and the marker begins on the next line.
    String.take 2 (String.drop (String.length prior - 1) out) `shouldEqual` "\n\n"
    String.take (String.length beginMarker) (String.drop (String.length prior + 1) out)
      `shouldEqual` beginMarker

  it "leaves a blank line between prior content and the marker (no trailing newline)" do
    let prior = "hello"
        out   = upsertPipelineSection payload prior
    -- Expect "hello\n\n<!-- ..."
    String.take (String.length prior + 2) out `shouldEqual` (prior <> "\n\n")

  it "replaces existing markers in-place, leaving surrounding content byte-identical" do
    let prior = "BEFORE\n" <> beginMarker <> "\nold stale content\n" <> endMarker <> "\nAFTER"
        out   = upsertPipelineSection payload prior
    -- The bytes before the first marker survive intact.
    String.take (String.length "BEFORE\n") out `shouldEqual` "BEFORE\n"
    -- The bytes after the end marker survive intact.
    (String.stripSuffix (Pattern "\nAFTER") out /= Nothing) `shouldEqual` true
    -- The stale content is gone.
    out `shouldSatisfy` (not <<< contains (Pattern "old stale content"))
    -- And the new section is present.
    out `shouldSatisfy` contains (Pattern "**Execution order:** physics → damage → cleanup")

  it "leaves a different pipeline's marker block untouched and appends its own" do
    let otherBegin = "<!-- pure-ecs:otherPipe:begin -->"
        otherEnd   = "<!-- pure-ecs:otherPipe:end -->"
        prior      = "Top\n" <> otherBegin <> "\nother body\n" <> otherEnd <> "\nTail"
        out        = upsertPipelineSection payload prior
    out `shouldSatisfy` contains (Pattern (otherBegin <> "\nother body\n" <> otherEnd))
    out `shouldSatisfy` contains (Pattern beginMarker)

  it "is idempotent: applying twice yields the same string as applying once" do
    let prior = "# Header\n\nintro\n"
        once  = upsertPipelineSection payload prior
        twice = upsertPipelineSection payload once
    twice `shouldEqual` once

  it "renders an empty step list as *(no systems)*" do
    let out = upsertPipelineSection (payload { stepNames = [] }) ""
    out `shouldSatisfy` contains (Pattern "**Execution order:** *(no systems)*")
