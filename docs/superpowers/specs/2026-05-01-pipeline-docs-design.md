# Pipeline Documentation Generator — Design

**Date:** 2026-05-01
**Status:** Approved, ready for implementation planning
**Version target:** pure-ecs 3.3.0

## Goal

Generate markdown documentation for ECS games that catalogs systems, their reads/writes, execution order, and inter-system data flow.

## Hard requirements

1. **Impossible to drift.** The value used to run the game must be the same value used to generate docs. There is no separate manifest, registry, or annotation file that can fall out of sync.
2. **Zero runtime cost.** Game-loop performance must not regress. Documentation work happens only when `documentPipeline` is called.
3. **Single learning surface.** `ECS.Examples.SimpleExample` migrates to use the new API and emits its own docs; learners see one cohesive walkthrough.
4. **Markdown output**, with mermaid diagrams where they earn their place.
5. **No game logic in docs** — structure only.

## Non-goals

- HTML output.
- Multi-pipeline index pages (users concatenate per-pipeline output themselves).
- Doc tracking for anonymous/inline systems (a pipeline step *must* be named).
- Sequence diagrams (the per-system reads/writes section already conveys this).
- Rendering arbitrary return types (`a`) — the source code is authoritative.
- Mermaid theming.
- Type-level union of pipeline-wide reads/writes (computable but unused by the doc generator; defer until parallel-execution analysis needs it).

## Architecture

Two new modules. Existing `ECS.System` is unchanged; bare `System reads writes a` remains usable.

### `ECS.Pipeline`

Defines the named-system / pipeline data model and the runtime path.

```purescript
-- A System tagged with a compile-time name.
newtype NamedSystem (name :: Symbol) (reads :: Row Type) (writes :: Row Type) a
  = NamedSystem (System reads writes a)

-- A composed pipeline.
--   pname  : the pipeline's own name
--   steps  : type-level cons list of step descriptors
--   a      : final return type (the result of the last step)
data Pipeline (pname :: Symbol) (steps :: PipelineList) a

-- Type-level list of pipeline steps. Each step carries its name + reads + writes
-- as types, so the doc generator can walk this structure.
foreign import data PipelineList :: Type
foreign import data PNil  :: PipelineList
foreign import data PCons :: Symbol -> Row Type -> Row Type -> PipelineList -> PipelineList
```

**Constructors:**

```purescript
-- Wrap a System with a name. Visible type application picks the name.
named
  :: forall @name r w a
   . IsSymbol name
  => System r w a
  -> NamedSystem name r w a

-- Start a pipeline from its first step.
pipeline
  :: forall @pname name r w a
   . IsSymbol pname
  => NamedSystem name r w a
  -> Pipeline pname (PCons name r w PNil) a

-- Append a step. Forms a snoc-list at the type level so the head of the
-- list is the FIRST step run, matching reading order.
infixl 1 appendStep as >->

appendStep
  :: forall pname steps name r w a b
   . Pipeline pname steps a
  -> NamedSystem name r w b
  -> Pipeline pname (Snoc steps name r w) b
```

**Runtime path:**

```purescript
runPipeline
  :: forall pname steps a
   . Pipeline pname steps a
  -> World
  -> { world :: World, result :: a }
```

`Pipeline` at runtime is a thin wrapper around the existing `System` machinery (sequence the inner `State World a` actions; return the last result). The new types are phantom-only on the runtime hot path.

### `ECS.Docs`

Defines the documentation path. Walks the type-level `PipelineList` via a class with one instance per constructor (`PNil`, `PCons`), pulling each step's name plus `reads` and `writes` rows; the rows reduce to `Array String` via `RowToList` + `IsSymbol`.

```purescript
documentPipeline
  :: forall pname steps a
   . IsSymbol pname
  => DocumentSteps steps
  => Pipeline pname steps a
  -> String

class DocumentSteps (steps :: PipelineList) where
  collectSteps :: Proxy steps -> Array StepDoc

type StepDoc =
  { name   :: String
  , reads  :: Array String
  , writes :: Array String
  }
```

`collectSteps` produces the ordered list. From there the markdown renderer is pure-data → `String`: no further type-class plumbing.

## What gets emitted

Per pipeline, in this order:

```markdown
# Pipeline: <pname>

## Execution order
1. <step1>
2. <step2>
…

## Data flow
```mermaid
flowchart LR
    <writer> -->|<component>| <reader>
    …
```

## Systems

### <step1>
- **Reads:** `c1`, `c2`
- **Writes:** `c1`

### <step2>
…

## Components touched
| Component | Read by | Written by |
|-----------|---------|------------|
| c1        | step1   | step1      |
| c2        | step1   | —          |
| …         | …       | …          |
```

Data-flow edges: for each component `c`, emit an edge `writer -->|c| reader` for every (writer, reader) pair where the writer step appears strictly before the reader step in execution order. No transitive deduplication for v1 (cheap to add later).

## The tricky type-level stuff — proven out first

This is where the design lives or dies. We prove these work before doing anything else:

### T1. Type-level snoc list of steps

`PipelineList` is the spine. Two interpretations were considered:

- **Cons (head = first step):** Reads naturally; appending is O(n) at the type level. Walking `PCons name r w rest` for documentation is straightforward.
- **Snoc (head = last step):** Appending is O(1) at the type level; walking is naturally last-to-first.

We use **Cons with type-level Snoc append**. The `Snoc` operation is implemented as a closed type family / class:

```purescript
class SnocStep (xs :: PipelineList) (n :: Symbol) (r :: Row Type) (w :: Row Type)
               (ys :: PipelineList) | xs n r w -> ys

instance SnocStep PNil n r w (PCons n r w PNil)
instance SnocStep tail n r w tail'
      => SnocStep (PCons n0 r0 w0 tail) n r w (PCons n0 r0 w0 tail')
```

`appendStep` then carries a `SnocStep steps name r w steps'` constraint. **Validation goal:** a 3-step chain `pipeline @"p" s1 >-> s2 >-> s3` infers to `Pipeline "p" (PCons "s1" r1 w1 (PCons "s2" r2 w2 (PCons "s3" r3 w3 PNil))) a` with no ambiguity errors.

### T2. Walking `PipelineList` to collect docs

```purescript
class DocumentSteps (steps :: PipelineList) where
  collectSteps :: Proxy steps -> Array StepDoc

instance DocumentSteps PNil where
  collectSteps _ = []

instance
  ( IsSymbol name
  , RowToList r rl
  , RowToList w wl
  , ExtractLabels rl
  , ExtractLabels wl
  , DocumentSteps rest
  ) => DocumentSteps (PCons name r w rest) where
  collectSteps _ =
    [ { name:   reflectSymbol (Proxy :: Proxy name)
      , reads:  extractLabels (Proxy :: Proxy rl)
      , writes: extractLabels (Proxy :: Proxy wl)
      } ] <> collectSteps (Proxy :: Proxy rest)
```

`ExtractLabels` already exists in `ECS.Query` for the same purpose; we reuse it (re-export if needed).

**Validation goal:** `collectSteps (Proxy :: Proxy <known 3-step type>)` returns the three `StepDoc` records in order, with reads and writes correctly populated.

### T3. Visible type applications for the user surface

`named @"physics" $ do ...` and `pipeline @"gameTick" ...` need to resolve cleanly. The PureScript compiler supports `@`-prefix visible type apps; the existing `queryFor @(...)` uses the same feature, so we know the version supports it.

### T4. Empty pipelines forbidden

There's no public constructor that produces `Pipeline pname PNil a`. `pipeline` requires a first step. `>->` only appends. **Validation goal:** attempt to construct an empty pipeline; expect a type error.

**De-risking order in the implementation plan:** T1 → T2 first as a tiny standalone sketch (a throwaway file or a REPL session). Only after both type-check and produce expected output do we wire them into `ECS.Pipeline` / `ECS.Docs` and start the migration.

**Bias toward compiling code over correct code at every step.** The implementation plan is structured so that after every step, `spago build` succeeds and `spago test` *runs* (even if individual new assertions fail). Failing tests are fine; failing-to-compile is not. This applies especially to the type-level steps below — we want to see real compiler error messages on real test invocations as early as possible, because that's the fastest feedback loop for the tricky stuff.

## Avoiding stuck states

Type-level programming is where this work most easily stalls. Concrete preventative measures:

**Annotate aggressively in the scratch phase (Step 0).** Don't trust inference. Write the full `Pipeline pname (PCons …) a` type on every binding. If the compiler still complains, the error points at one line, not at the inference horizon.

**One type-class instance change per build.** When iterating on `SnocStep` or `DocumentSteps`, save and rebuild after every individual change. A green build on a one-line change tells you exactly which line broke things; a red build on a five-line change can take an hour to bisect.

**Escape hatches per risk:**

| Risk | Fallback |
|------|----------|
| `SnocStep` instance resolution gets confused on chained `>->` | Switch to a **cons-prepend** API: `step >-> existingPipeline` adds to the front, then reverse the `Array StepDoc` once at doc time. Loses the natural left-to-right reading order in user code, but eliminates the type-level snoc entirely. |
| `>->` chains produce inscrutable type errors for end users | Document the explicit-type-annotation pattern in `CLAUDE.md` and provide a `mkPipeline3`, `mkPipeline4`, … helper family for common arities. Not pretty, but unblocks. |
| `RowToList` over an empty `writes` row (`()`) picks the wrong instance | Define `ExtractLabels Nil` explicitly as `[]` and verify with a unit test in Step 0 before relying on it. |
| Visible type application `@"foo"` interacts badly with multi-parameter constraints | Drop visible type apps; pass `Proxy :: Proxy "foo"` explicitly. Uglier call sites but bulletproof. |
| `node-fs` not in the package set | Skip the file write in `runExample` for v1; `log` the markdown to stdout and let the user redirect. Generated `docs/example-pipeline.md` becomes a manually-committed snapshot of that output (still impossible to drift in *content*, just in the *commit cadence*). |

**Ground rule: do not rewrite working code to be "more elegant" mid-stream.** If Step 1a compiles with placeholder `runPipeline`, do not also "improve" `named` while you're in there. Each step is small on purpose.

**If you spend 30 minutes fighting a type error and aren't measurably closer, stop and pick the fallback.** The goal is shipping documentation generation, not winning a type-level chess game.

## Migration plan

The migration is the second-biggest risk after the type-level work. Done in narrow, verifiable steps:

### Step 0 — De-risk type-level machinery (above)

Prove T1 and T2 in a scratch module before touching anything else. Discard the scratch.

### Step 1 — Land `ECS.Pipeline` (runtime only, no docs)

**Step 1a — Skeleton that compiles.** Add `ECS.Pipeline` containing only the type definitions (`NamedSystem`, `Pipeline`, `PipelineList`, `PNil`, `PCons`, `SnocStep` class) plus `named`, `pipeline`, `>->`, and `runPipeline` with placeholder implementations (`runPipeline _ w = { world: w, result: undefined }` is fine). Add `test/PipelineSpec.purs` with one test that constructs a 3-step pipeline value and binds it to a variable of the expected `Pipeline pname (PCons "s1" … (PCons "s2" … (PCons "s3" … PNil))) a` type. Wire the spec into `test/Main.purs`. **Exit criterion:** `spago build` and `spago test` both succeed; the new spec shows up in test output (it can fail at runtime — type-checking is what matters here).

**Step 1b — Real implementations.** Replace placeholders with the real runtime: `runPipeline` sequences the inner `State World a` actions and returns the last result. Flesh out the spec:
  - Composition order is preserved (run a 3-step pipeline; assert side effects fire in order).
  - `runPipeline (pipeline @"p" (named @"x" s))` matches `runSystem s` for an arbitrary `s`.
  - Result of the pipeline is the result of the last step.

**Exit criterion for Step 1:** `spago test` is fully green for the new spec.

### Step 2 — Land `ECS.Docs`

**Step 2a — Skeleton that compiles.** Add `ECS.Docs` with the `DocumentSteps` class, both instances, `StepDoc`, and `documentPipeline` returning a placeholder string (e.g. `show (collectSteps …)`). Add `test/DocsSpec.purs` with one test that calls `collectSteps` on a known pipeline type and binds the result. Wire into `test/Main.purs`. **Exit criterion:** `spago build` and `spago test` succeed; the new spec runs (assertions can fail).

**Step 2b — Real renderer.** Implement the markdown renderer (pure data → `String`). Flesh out the spec:
  - `collectSteps` on a known pipeline type returns the expected ordered `Array StepDoc`.
  - The markdown renderer produces deterministic output for a fixture pipeline (whole-string assertion against an inline expected value).
  - Data-flow edges enumerate all (writer, reader) pairs strictly in execution order, for each component.

**Exit criterion for Step 2:** `spago test` is fully green for the new spec.

At this point both new modules are tested and shippable; nothing in the existing codebase has changed.

### Step 3 — Migrate `SimpleExample`

- Wrap each of `physicsSystem`, `damageSystem`, `cleanupSystem` with `named @"…" …` (their definitions stay byte-identical; only the *site* that combines them changes).
- Replace the inline `do`-block in `gameTick` with a `Pipeline "gameTick" …`.
- Replace the `runSystem` call in `runSimulation` with `runPipeline`.
- Add a final block to `runExample` that:
  1. Calls `documentPipeline gameTick`.
  2. Writes the result to `docs/example-pipeline.md` using `Node.FS.Sync.writeTextFile`.
- Commit the generated `docs/example-pipeline.md` so it's visible in the repo without running the example.

### Step 4 — Verify nothing regressed

- `spago test` passes (existing 144 tests, plus new pipeline + docs tests).
- `spago run` for the example produces the same simulation output as before AND writes/refreshes `docs/example-pipeline.md` matching the committed copy.
- Update `CLAUDE.md` and `README.md` with a short "Documenting your game" section pointing at `ECS.Pipeline`, `ECS.Docs`, and the example doc.

### Step 5 — Optional follow-up (not part of this plan)

A separate pitch could add an HTML renderer, a multi-pipeline index, or static-asset publishing. Out of scope here.

## Files touched

**New:**
- `src/ECS/Pipeline.purs`
- `src/ECS/Docs.purs`
- `test/PipelineSpec.purs`
- `test/DocsSpec.purs`
- `docs/example-pipeline.md` (generated, committed)

**Modified:**
- `src/ECS/Examples/SimpleExample.purs` (migration; logic unchanged)
- `test/Main.purs` (register new specs)
- `CLAUDE.md` (add doc-generation section, bump version to 3.3.0)
- `src/ECS/CLAUDE.md` (sync)
- `README.md` (mention doc generation)
- `spago.yaml` (add `node-fs` dep for the example's file write — verify it isn't already transitive)

## Open questions resolved during brainstorming

- **Q1 — Doc scope:** systems, execution order, data flow. (Option B from brainstorm.)
- **Q2 — Registration:** pipeline values are the manifest; no separate list.
- **Q3 — Markdown content:** keep data-flow graph; drop sequence diagram and return-type rendering.
- **Q4 — Example shape:** migrate `SimpleExample` for one cohesive walkthrough.
