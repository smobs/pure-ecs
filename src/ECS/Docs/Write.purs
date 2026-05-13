-- | Persist pipeline documentation to disk and announce it in agent-spec files.
-- |
-- | `ECS.Docs.documentPipeline` is pure: it turns a `Pipeline` value into a
-- | markdown `String`. This module is the I/O companion. It writes that
-- | markdown to a file, then upserts a short marker block into any
-- | `AGENTS.md` / `CLAUDE.md` / `GEMINI.md` it finds in the current working
-- | directory so an agent reading those files learns the docs exist and where
-- | to find them.
-- |
-- | The marker block is per-pipeline (parameterised by the pipeline's `pname`),
-- | so multiple pipelines coexist in the same agent file without overwriting
-- | each other. Re-running with unchanged inputs is a byte-for-byte no-op.
module ECS.Docs.Write
  ( agentFiles
  , upsertPipelineSection
  , writePipelineDocs
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..))
import Data.String as String
import ECS.Docs (class DocumentSteps, collectSteps, documentPipeline)
import ECS.Pipeline (Pipeline)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Type.Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | Files we look for in cwd. Each is updated only if it already exists —
-- | we never create agent files, since their presence is the opt-in signal.
agentFiles :: Array String
agentFiles =
  [ "AGENTS.md"
  , "CLAUDE.md"
  , "GEMINI.md"
  ]

-- | Generate pipeline documentation, write it to `docPath`, and upsert a
-- | per-pipeline marker block into every agent-spec file in `agentFiles`
-- | that exists in the current working directory.
writePipelineDocs
  :: forall pname steps a
   . IsSymbol pname
  => DocumentSteps steps
  => String
  -> Pipeline pname steps a
  -> Effect Unit
writePipelineDocs docPath pipe = do
  writeTextFile UTF8 docPath (documentPipeline pipe)
  let payload =
        { pname:     reflectSymbol (Proxy :: Proxy pname)
        , docPath
        , stepNames: map _.name (collectSteps (Proxy :: Proxy steps))
        }
  for_ agentFiles \path -> do
    present <- exists path
    when present do
      contents <- readTextFile UTF8 path
      let updated = upsertPipelineSection payload contents
      when (updated /= contents) do
        writeTextFile UTF8 path updated

-- | Pure splice: given the section payload and an existing file body, return
-- | the file body with this pipeline's marker block either replaced in-place
-- | (markers already present) or appended (markers missing). Idempotent:
-- | feeding the result back in yields the same string.
upsertPipelineSection
  :: { pname :: String, docPath :: String, stepNames :: Array String }
  -> String
  -> String
upsertPipelineSection { pname, docPath, stepNames } existing =
  case String.indexOf (Pattern beginMarker) existing,
       String.indexOf (Pattern endMarker) existing of
    Just startIx, Just endIxStart | startIx < endIxStart ->
      let endIx  = endIxStart + String.length endMarker
          before = String.take startIx existing
          after  = String.drop endIx existing
      in before <> block <> after
    _, _ ->
      existing <> separator <> block <> "\n"
  where
    beginMarker = "<!-- pure-ecs:" <> pname <> ":begin -->"
    endMarker   = "<!-- pure-ecs:" <> pname <> ":end -->"
    block       = beginMarker <> "\n" <> body <> endMarker

    body = String.joinWith "\n"
      [ ""
      , "### ECS pipeline: `" <> pname <> "`"
      , ""
      , "Auto-generated docs: [" <> docPath <> "](" <> docPath <> ")"
      , ""
      , "**Execution order:** " <> formatExecution stepNames
      , ""
      , "Managed by `ECS.Docs.Write`; do not hand-edit."
      , ""
      ]

    formatExecution [] = "*(no systems)*"
    formatExecution xs = String.joinWith " → " xs

    separator
      | existing == ""                                            = ""
      | isJust (String.stripSuffix (Pattern "\n") existing)       = "\n"
      | otherwise                                                 = "\n\n"
