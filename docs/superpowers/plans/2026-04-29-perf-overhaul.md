# pure-ecs Performance Overhaul Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Take pure-ecs from "struggles at a few hundred entities" to comfortably handling tens of thousands of entities by eliminating the dominant per-entity costs identified in the multi-agent performance review.

**Architecture:** Five compounding fixes:
1. Configure `purs-backend-es` to inline dictionary dispatch (free 2–5×).
2. Add a benchmark harness so every subsequent change is measured.
3. Add an in-place `setComponent` fast path so value updates skip archetype migration (the smoking gun: today every `updateComponent_` call rebuilds two archetypes).
4. Fix `runQuery` quadratic patterns: thread `archId` instead of reverse-looking it up, hoist archetype/storage lookups out of per-entity-per-component loops, kill `acc <> [x]` accumulation.
5. Re-key archetypes and the query cache by `ComponentMask :: Int` so hot-path lookups stop hashing strings.

The misc cleanups (stale `output/`, dead `range`) are folded into the relevant tasks. Mutable column buffers (`STArray`, typed buffers) are explicitly out of scope — that's a research-level rewrite for 100k+ scale; this plan targets the reachable 100× win without touching purity.

**Tech Stack:** PureScript 0.15.x, spago (next), `purs-backend-es`, `purescript-spec` for tests, `Effect.Now` for benchmark timing, esbuild for bundling.

---

## File Structure

**New files:**
- `bench/Main.purs` — benchmark harness (Effect.Now timing of representative ECS workloads)
- `bench/ECS/Bench.purs` — benchmark scenarios (spawn, query, update, modify)

**Modified files:**
- `spago.yaml` — add backend block, add bench package
- `package.json` — add `purs-backend-es` devDep, add `bench` script, fix build:demo
- `build-demo.js` — point at `output-es/` instead of `output/`
- `src/ECS/Component.purs` — add `setComponentPure`, drop dead `range`, keep migrations correct
- `src/ECS/System.purs` — rewrite `updateComponent`/`_`/`modifyComponent`/`_` to use `setComponentPure`
- `src/ECS/World.purs` — change `archetypes :: Map ComponentMask Archetype`, change `queryCache :: Map (Tuple Int Int) CachedQueryResult`, drop `ArchetypeId = String` from hot path
- `src/ECS/Query.purs` — rewrite `runQuery`/`runQueryCached` to thread mask, hoist archetype/storage refs, replace `<>` accumulators
- `src/ECS/Examples/SimpleExample.purs` and `src/ECS/Examples/WebDemo.purs` — adjust to any signature changes
- `test/*.purs` — adjust to signature changes

---

## Conventions

- One commit per task. Use Conventional Commits: `feat:`, `fix:`, `perf:`, `chore:`, `test:`, `bench:`.
- Run `spago test` after every code change. Don't proceed until green.
- After backend optimiser is configured (Task 2), all benchmark numbers are measured against the optimised build.
- Record bench results in PR description / commit body for each perf task — the bench harness exists to substantiate claims, not just to exist.

---

## Task 1: Clean stale build output

The compiled `output/` references symbols (`parseArchetypeId`, `archetypeMatches`) that no longer exist in source. Any benchmarks run against it would be measuring a phantom build.

**Files:**
- Run: `npm run clean`
- Run: `spago build`

- [ ] **Step 1: Clean and rebuild**

```bash
cd /home/toby/pure-ecs
npm run clean
spago build
spago test
```

Expected: `spago build` succeeds, `spago test` reports all tests passing (110+ tests).

- [ ] **Step 2: Commit (no code changed, but the lockfile/output state is recorded by the next change)**

No commit needed — this is just a known-good baseline. Note the test count and timing in your notes for later comparison.

---

## Task 2: Configure `purs-backend-es`

The default PureScript JS backend ships uninlined dictionary thunks. `purs-backend-es` inlines and uncurries — typically 2–5× on hot loops. This is a free win and must land first so all subsequent benchmarks reflect realistic perf.

**Files:**
- Modify: `package.json`
- Modify: `spago.yaml`
- Modify: `build-demo.js:9` (change import path)

- [ ] **Step 1: Install `purs-backend-es`**

```bash
cd /home/toby/pure-ecs
npm install --save-dev purs-backend-es
```

Expected: `purs-backend-es` appears in `package.json` `devDependencies`.

- [ ] **Step 2: Configure spago to use the backend**

Edit `/home/toby/pure-ecs/spago.yaml` so the top-level looks like this:

```yaml
workspace:
  packageSet:
    registry: 56.4.0
  backend:
    cmd: purs-backend-es
    args:
      - build

package:
  name: pure-ecs
  dependencies:
    - arrays
    - console
    - effect
    - either
    - foreign
    - foreign-object
    - lists
    - maybe
    - ordered-collections
    - prelude
    - record
    - transformers
    - tuples
    - typelevel-prelude
  test:
    main: Test.Main
    dependencies:
      - spec
```

- [ ] **Step 3: Update `build-demo.js` to consume the optimised output**

The optimised backend writes to `output-es/` by default. Edit `/home/toby/pure-ecs/build-demo.js` and change line 9 from:

```javascript
import * as WebDemo from './output/ECS.Examples.WebDemo/index.js';
```

to:

```javascript
import * as WebDemo from './output-es/ECS.Examples.WebDemo/index.js';
```

- [ ] **Step 4: Add `clean` cover for the new output dir**

Edit `/home/toby/pure-ecs/package.json` `scripts.clean` from:

```json
"clean": "rm -rf output .spago"
```

to:

```json
"clean": "rm -rf output output-es .spago"
```

- [ ] **Step 5: Verify build & tests**

```bash
cd /home/toby/pure-ecs
npm run clean
spago build
spago test
```

Expected: `spago build` produces `output-es/` populated. `spago test` passes all tests. If `purs-backend-es` complains about a missing CLI, reinstall via `npm i -D purs-backend-es` and re-run.

- [ ] **Step 6: Verify the demo builds**

```bash
cd /home/toby/pure-ecs
npm run build:demo
```

Expected: `docs/app.js` is regenerated and noticeably smaller.

- [ ] **Step 7: Commit**

```bash
cd /home/toby/pure-ecs
git add package.json package-lock.json spago.yaml build-demo.js
git commit -m "perf: configure purs-backend-es optimising backend

Default purs JS output ships uninlined typeclass dictionaries and
curried wrappers — purs-backend-es inlines and uncurries these,
typically 2–5x on tight loops. Demo build now consumes output-es/."
```

---

## Task 3: Add benchmark harness

We need numbers before changing anything else. A simple `Effect.Now`-based timer is enough — we're looking for ratios, not microsecond accuracy.

**Files:**
- Create: `bench/Main.purs`
- Create: `bench/ECS/Bench.purs`
- Modify: `spago.yaml` (add `bench` section)
- Modify: `package.json` (add `bench` script)

- [ ] **Step 1: Add `bench` workspace entry to spago**

Edit `/home/toby/pure-ecs/spago.yaml` and add a `bench` block under the package's already-present `test` block:

```yaml
package:
  name: pure-ecs
  dependencies:
    - arrays
    - console
    - effect
    - either
    - foreign
    - foreign-object
    - lists
    - maybe
    - ordered-collections
    - prelude
    - record
    - transformers
    - tuples
    - typelevel-prelude
  test:
    main: Test.Main
    dependencies:
      - spec
  bundle:
    module: Bench.Main
    outfile: bench-out.js
    platform: node
```

- [ ] **Step 2: Create `bench/ECS/Bench.purs`**

Create `/home/toby/pure-ecs/bench/ECS/Bench.purs`:

```purescript
module Bench.ECS.Bench
  ( Scenario
  , spawn1k
  , queryAll1k
  , update1k
  , modify1k
  , fullTick1k
  ) where

import Prelude

import Control.Monad.State (execState)
import Data.Array (range)
import Data.Foldable (for_)
import Data.Int (toNumber)
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, modifyComponent_, queryFor)
import ECS.World (emptyWorld, spawnEntity, World)
import Type.Proxy (Proxy(..))

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health   = { current :: Int, max :: Int }

type Scenario =
  { name  :: String
  , build :: World
  , run   :: World -> World
  }

buildWorld :: Int -> World
buildWorld n = execState
  (for_ (range 0 (n - 1)) \i ->
      spawnEntity
        <+> (Proxy :: _ "position") := { x: toNumber i, y: toNumber i }
        <+> (Proxy :: _ "velocity") := { x: 1.0, y: 1.0 }
        <+> (Proxy :: _ "health")   := { current: 100, max: 100 })
  emptyWorld

movementSystem :: System ( position :: Position, velocity :: Velocity )
                          ( position :: Position )
                          Unit
movementSystem = do
  rs <- queryFor @( position :: Position, velocity :: Velocity )
  for_ rs \r ->
    modifyComponent_ (Proxy :: _ "position")
      (\p -> { x: p.x + r.components.velocity.x
             , y: p.y + r.components.velocity.y })
      r.entity

spawn1k :: Scenario
spawn1k =
  { name: "spawn 1000 entities (3 components each)"
  , build: emptyWorld
  , run: \_ -> buildWorld 1000
  }

queryAll1k :: Scenario
queryAll1k =
  { name: "query+update (pos, vel) over 1000 entities"
  , build: buildWorld 1000
  , run: \w -> (runSystem movementSystem w).world
  }

update1k :: Scenario
update1k = queryAll1k { name = "update position via modifyComponent_ over 1000 entities" }

modify1k :: Scenario
modify1k = queryAll1k { name = "modify position over 1000 entities" }

fullTick1k :: Scenario
fullTick1k =
  { name: "10 movement ticks over 1000 entities"
  , build: buildWorld 1000
  , run: \w -> tickN 10 w
  }
  where
    tickN 0 w = w
    tickN k w = tickN (k - 1) (runSystem movementSystem w).world
```

- [ ] **Step 3: Create `bench/Main.purs`**

Create `/home/toby/pure-ecs/bench/Main.purs`:

```purescript
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

bench :: Scenario -> Effect Unit
bench scenario = do
  let _warm = scenario.run scenario.build
  totalRef <- Ref.new 0.0
  for_ (range 0 (iterations - 1)) \_ -> do
    t0 <- now
    let result = scenario.run scenario.build
    let _force = show (result.structuralVersion)
    t1 <- now
    let Milliseconds m0 = unInstant t0
    let Milliseconds m1 = unInstant t1
    Ref.modify_ (_ + (m1 - m0)) totalRef
  total <- Ref.read totalRef
  let avg = total / toNumber iterations
  log $ scenario.name <> ": " <> show (round avg) <> " ms (avg of " <> show iterations <> ")"

main :: Effect Unit
main = for_ scenarios bench
```

The `_force` line uses `world.structuralVersion` to force evaluation; if the field name has changed in later tasks, update accordingly.

- [ ] **Step 4: Add `datetime` and `now` deps if missing**

`Effect.Now` lives in `purescript-now`. Check if it's already a transitive dep:

```bash
cd /home/toby/pure-ecs
spago build 2>&1 | tail -20
```

If `Effect.Now` is missing, add `now` and `datetime` to dependencies in `spago.yaml`:

```yaml
package:
  name: pure-ecs
  dependencies:
    - arrays
    - console
    - datetime
    - effect
    - either
    - foreign
    - foreign-object
    - lists
    - maybe
    - now
    - ordered-collections
    - prelude
    - record
    - refs
    - transformers
    - tuples
    - typelevel-prelude
```

(Also add `refs` for `Effect.Ref`.)

- [ ] **Step 5: Add `bench` script in `package.json`**

In `/home/toby/pure-ecs/package.json` `scripts`, add:

```json
"bench": "spago bundle --module Bench.Main --outfile bench-out.cjs --platform node && node bench-out.cjs"
```

- [ ] **Step 6: Run benchmarks and capture baseline**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-baseline.txt
```

Expected: prints three lines like:
```
spawn 1000 entities (3 components each): NN ms (avg of 5)
query+update (pos, vel) over 1000 entities: NNNN ms (avg of 5)
10 movement ticks over 1000 entities: NNNNN ms (avg of 5)
```

The numbers will be ugly. That's the point — they are the baseline against which Tasks 4–9 are measured.

- [ ] **Step 7: Commit**

```bash
cd /home/toby/pure-ecs
git add bench/ spago.yaml package.json package-lock.json bench-baseline.txt
git commit -m "bench: add Effect.Now timing harness for ECS workloads

Three scenarios: 1k spawn, 1k query+update, 10 ticks over 1k.
Baseline numbers committed in bench-baseline.txt — every
subsequent perf change must be measured against these."
```

---

## Task 4: Add `setComponentPure` (in-place value write)

This is the smoking gun fix. Today, `updateComponent` runs `removeComponentPure` + `addComponentPure`, so updating one Number triggers full archetype migration: extract every component, splice every column, re-append every column. For N entities × C components, the per-tick cost is O(N²·C) array copies.

The phantom row type is unchanged in a value update — `Lacks/Cons` already prove the schema is invariant. So we can write directly to one component column at one row index. No migration. No structural-version bump.

**Files:**
- Modify: `src/ECS/Component.purs:24-27` (export list — add `setComponentPure`)
- Modify: `src/ECS/Component.purs:130` (after `with`/<+> definitions — add new `setComponentPure`)
- Modify: `src/ECS/Internal/ComponentStorage.purs:21` (export list — add `arrayUpdateAt`)
- Modify: `src/ECS/Internal/ComponentStorage.purs` (add `arrayUpdateAt` impl)
- Test: `test/ComponentSpec.purs`

- [ ] **Step 1: Write the failing test**

Add this test in `/home/toby/pure-ecs/test/ComponentSpec.purs` inside the existing `componentSpec :: Spec Unit` `describe` block. Find the existing `describe "ECS.Component"` and add a new nested `describe`:

```purescript
    describe "setComponentPure" do
      it "writes a new value without archetype migration" do
        let world = emptyWorld
            { world: w0, entity: e0 } = spawnEntityPure world
            { world: w1, entity: e1 } = addComponentPure (Proxy :: _ "position") {x: 1.0, y: 2.0} e0 w0
            archIdBefore = Map.lookup (entityIndex (unEntity e1)) w1.entityLocations
            structVerBefore = w1.structuralVersion
            { world: w2, entity: _ } = setComponentPure (Proxy :: _ "position") {x: 99.0, y: 99.0} e1 w1
            archIdAfter = Map.lookup (entityIndex (unEntity e1)) w2.entityLocations
            structVerAfter = w2.structuralVersion
            newVal = getComponentPure (Proxy :: _ "position") e1 w2
        archIdBefore `shouldEqual` archIdAfter
        structVerBefore `shouldEqual` structVerAfter
        newVal `shouldEqual` Just {x: 99.0, y: 99.0}
```

You will need these imports near the top of `ComponentSpec.purs`:
```purescript
import Data.Map as Map
import ECS.Component (setComponentPure, addComponentPure, getComponentPure)
import ECS.Entity (entityIndex)
import ECS.World (emptyWorld, spawnEntityPure, unEntity)
```

(Some may already exist — don't duplicate.)

- [ ] **Step 2: Run the test to verify it fails**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: compile error `Unknown identifier setComponentPure`. That's the failing-test signal.

- [ ] **Step 3: Add `arrayUpdateAt` to ComponentStorage**

Edit `/home/toby/pure-ecs/src/ECS/Internal/ComponentStorage.purs`. In the export list (lines 8-26), add `arrayUpdateAt`:

```purescript
module ECS.Internal.ComponentStorage
  ( ComponentStorage
  , ComponentArray
  , empty
  , lookup
  , insert
  , mapWithKey
  , fold
  , filterKeys
  -- Component array operations
  , emptyArray
  , arrayAppend
  , arrayIndex
  , arrayFromSingleton
  , arrayRemoveAt
  , arraySwapRemoveAt
  , arrayUpdateAt
  , componentToForeign
  , componentFromForeign
  ) where
```

Then add this function after `arraySwapRemoveAt` (after line 160):

```purescript
-- | Replace the element at index `idx` with a new value.
-- |
-- | Returns the array unchanged if the index is out of bounds.
-- | This is the in-place column write used by setComponentPure.
arrayUpdateAt :: forall a. Int -> a -> ComponentArray -> ComponentArray
arrayUpdateAt idx newValue arr =
  case Array.updateAt idx (unsafeToForeign newValue) arr of
    Nothing -> arr
    Just updated -> updated
```

- [ ] **Step 4: Add `setComponentPure` to Component.purs**

Edit `/home/toby/pure-ecs/src/ECS/Component.purs`. In the export list (lines 12-27), add `setComponentPure`:

```purescript
module ECS.Component
  ( addComponent
  , removeComponent
  , getComponent
  , hasComponent
  -- Chaining combinator
  , with
  , (<+>)
  -- Component pairing for elegant syntax
  , ComponentPair(..)
  , (:=)
  -- Pure versions (for internal use)
  , addComponentPure
  , removeComponentPure
  , getComponentPure
  , setComponentPure
  ) where
```

Then add this function after `getComponentPure` (around line 348). It uses `Cons` (component must already exist) — the schema is unchanged, so no `Lacks` and no row-type transformation:

```purescript
-- | In-place component value update — no archetype migration.
-- |
-- | The Cons constraint proves the component already exists, so the entity's
-- | row type is invariant. We do a single Array.updateAt on the component
-- | column at the entity's row index. No structural change → no cache
-- | invalidation, no archetype bookkeeping.
-- |
-- | Returns the world unchanged if the entity is invalid or its archetype/
-- | column/index is unexpectedly missing (these cannot happen for a valid
-- | Entity r and existing component, but we degrade gracefully).
setComponentPure :: forall r label a trash.
  IsSymbol label =>
  Cons label a trash r =>
  Proxy label ->
  a ->
  Entity r ->
  World ->
  { world :: World, entity :: Entity r }
setComponentPure labelProxy newValue entity world =
  let
    labelStr = reflectSymbol labelProxy
    entityId = unEntity entity
    idx      = entityIndex entityId
  in
    if not (validateEntity entityId world.entities) then
      { world, entity }
    else case Map.lookup idx world.entityLocations of
      Nothing -> { world, entity }
      Just archId -> case Map.lookup archId world.archetypes of
        Nothing -> { world, entity }
        Just arch -> case Map.lookup idx arch.entityPositions of
          Nothing -> { world, entity }
          Just pos -> case CS.lookup labelStr arch.storage of
            Nothing -> { world, entity }
            Just column ->
              let
                newColumn  = CS.arrayUpdateAt pos newValue column
                newStorage = CS.insert labelStr newColumn arch.storage
                newArch    = arch { storage = newStorage }
              in
                { world: world { archetypes = Map.insert archId newArch world.archetypes }
                , entity
                }
```

- [ ] **Step 5: Run the test to verify it passes**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass, including the new `setComponentPure` test.

- [ ] **Step 6: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/Component.purs src/ECS/Internal/ComponentStorage.purs test/ComponentSpec.purs
git commit -m "perf: add setComponentPure for in-place value writes

setComponentPure does a single Array.updateAt on one component column —
no archetype migration, no structural-version bump. The Cons constraint
proves the row type is invariant, so this is type-safe and equivalent
to remove+add for value updates, but O(1) in archetype size instead
of O(N*C)."
```

---

## Task 5: Wire `setComponentPure` into `updateComponent`/`updateComponent_`/`modifyComponent`/`modifyComponent_`

These four System helpers are the user-facing hot path. All four currently call `removeComponentPure` + `addComponentPure`. Replace them with `setComponentPure` (or `getComponentPure` + `setComponentPure` for the modify pair).

**Files:**
- Modify: `src/ECS/System.purs:28` (import — replace `addComponentPure, removeComponentPure` with `setComponentPure` for these helpers; keep `getComponentPure`)
- Modify: `src/ECS/System.purs:158-201, 245-285` (replace bodies)

- [ ] **Step 1: Update imports**

Edit `/home/toby/pure-ecs/src/ECS/System.purs` line 28 from:

```purescript
import ECS.Component (addComponentPure, removeComponentPure, getComponentPure)
```

to:

```purescript
import ECS.Component (getComponentPure, setComponentPure)
```

- [ ] **Step 2: Rewrite `updateComponent` body**

Replace the body of `updateComponent` (lines 158-163) with:

```purescript
updateComponent label newValue entity = state \world ->
  let { world: world' } = setComponentPure label newValue entity world
  in Tuple entity world'
```

Note the type signature stays the same — `Cons label a r' r => ... => Entity r -> System r' writes (Entity r)`. The constraint `Lacks label r'` is no longer used by the body but is harmless (it's still a valid constraint and existing callers depend on the signature shape).

**Wait — re-examine the type:** the existing signature is `Cons label a r' r => Cons label a trash writes => Lacks label r' => Proxy label -> a -> Entity r -> System r' writes (Entity r)`. The return is `System r' writes (Entity r)` — `r'` is the read row, `Entity r` is the result. This is correct: the entity's row type is unchanged in the result. Keep the existing constraints.

- [ ] **Step 3: Rewrite `updateComponent_` body**

Replace the body of `updateComponent_` (lines 196-201) with:

```purescript
updateComponent_ label newValue entity = state \world ->
  let { world: world' } = setComponentPure label newValue entity world
  in Tuple unit world'
```

- [ ] **Step 4: Rewrite `modifyComponent` body**

Replace the body of `modifyComponent` (lines 245-251) with:

```purescript
modifyComponent proxy f entity = state \world ->
  case getComponentPure proxy entity world of
    Nothing -> Tuple entity world
    Just value ->
      let { world: world' } = setComponentPure proxy (f value) entity world
      in Tuple entity world'
```

- [ ] **Step 5: Rewrite `modifyComponent_` body**

Replace the body of `modifyComponent_` (lines 279-285) with:

```purescript
modifyComponent_ proxy f entity = state \world ->
  case getComponentPure proxy entity world of
    Nothing -> Tuple unit world
    Just value ->
      let { world: world' } = setComponentPure proxy (f value) entity world
      in Tuple unit world'
```

- [ ] **Step 6: Run the existing tests**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass. The tests in `SystemSpec.purs` already cover `updateComponent` semantics; if any fail, the body rewrite has a typo — re-check.

- [ ] **Step 7: Re-run benchmarks**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-task5.txt
```

Expected: `query+update` and `10 movement ticks` scenarios are now **dramatically** faster than the baseline — typically 10–50× depending on N. If they're not faster, the rewrite has bugs (most likely: `setComponentPure` is silently degrading because the entity isn't being found).

- [ ] **Step 8: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/System.purs bench-after-task5.txt
git commit -m "perf: route updateComponent/modifyComponent through setComponentPure

The four System update helpers previously did remove+add for value
writes, triggering full archetype migration on every call inside a
for_ loop. Routing them through setComponentPure makes value writes
a single column update. See bench-after-task5.txt for measured impact."
```

---

## Task 6: Eliminate `findArchetypeId` from `runQuery` and replace `<> [x]` accumulators

`runQuery` line 175-182 reverse-looks-up an archetype's ID from its mask, scanning all archetypes for each match — O(A²). The same archetype IDs were already in scope at line 142. Just thread them through, like `runQueryCached` already does.

Also replace the four `acc <> [x]` quadratic accumulators (lines 142-146, 149-152, 259-263, 273-279) with `Array.filter`/`Array.mapMaybe`/`Array.concatMap`.

**Files:**
- Modify: `src/ECS/Query.purs:127-182` (rewrite `runQuery`)
- Modify: `src/ECS/Query.purs:234-281` (rewrite `runQueryCached`)

- [ ] **Step 1: Rewrite `runQuery`**

Replace the body of `runQuery` (lines 127-182, including the local `where`-bindings for `extractFromArchetype` and `findArchetypeId`) with:

```purescript
runQuery (Query q) world =
  let
    requiredResult = labelsToMaskStrict q.requiredLabels world
    excludedMask   = labelsToMask q.excludedLabels world
  in
    case requiredResult of
      Nothing -> []
      Just requiredMask ->
        let
          matches :: Array (Tuple ArchetypeId Archetype)
          matches = Array.filter
            (\(Tuple _ arch) -> archetypeMatchesMask arch.mask requiredMask excludedMask)
            (Map.toUnfoldable world.archetypes)
        in
          Array.concatMap
            (\(Tuple archId arch) -> extractFromArchetype archId arch world)
            matches
  where
    extractFromArchetype :: ArchetypeId -> Archetype -> World -> Array (QueryResult required)
    extractFromArchetype archId arch world' =
      map (\entityId ->
        { entity: wrapEntity entityId
        , components: readComponents (Proxy :: Proxy rl) archId entityId world'
        }
      ) arch.entities
```

You will need `import Data.Array as Array` and `import Data.Tuple (Tuple(..))` at the top of the file. Verify these imports already exist (Tuple is imported via `Data.Tuple.Nested`); add what's missing. The local `findArchetypeId` is **deleted entirely**.

- [ ] **Step 2: Rewrite `runQueryCached`**

Replace the body of `runQueryCached` (lines 234-281) with the version below. Note that `extractFromArchetypeCached` already takes `archId` directly — the only changes here are replacing the two `acc <> [x]` patterns:

```purescript
runQueryCached (Query q) world =
  let
    requiredResult = labelsToMaskStrict q.requiredLabels world
    excludedMask   = labelsToMask q.excludedLabels world
  in
    case requiredResult of
      Nothing -> { results: [], world }
      Just requiredMask ->
        let
          cacheKey    = makeQueryCacheKey requiredMask excludedMask
          cacheResult = checkCache cacheKey world

          { matchingArchIds, world': worldAfterCache } = case cacheResult of
            Just archIds ->
              { matchingArchIds: archIds, world': world }
            Nothing ->
              let
                archIds = Array.mapMaybe
                  (\(Tuple archId arch) ->
                      if archetypeMatchesMask arch.mask requiredMask excludedMask
                        then Just archId
                        else Nothing)
                  (Map.toUnfoldable world.archetypes)
                newCacheEntry = { matchingArchetypes: archIds, version: world.structuralVersion }
                updatedCache  = Map.insert cacheKey newCacheEntry world.queryCache
              in
                { matchingArchIds: archIds
                , world': world { queryCache = updatedCache }
                }

          results = Array.concatMap
            (\archId -> case Map.lookup archId worldAfterCache.archetypes of
                Nothing   -> []
                Just arch -> extractFromArchetypeCached archId arch worldAfterCache)
            matchingArchIds
        in
          { results, world: worldAfterCache }
  where
    checkCache :: QueryCacheKey -> World -> Maybe (Array ArchetypeId)
    checkCache key w = case Map.lookup key w.queryCache of
      Nothing -> Nothing
      Just cached ->
        if cached.version == w.structuralVersion
          then Just cached.matchingArchetypes
          else Nothing

    extractFromArchetypeCached :: ArchetypeId -> Archetype -> World -> Array (QueryResult required)
    extractFromArchetypeCached archId arch world' =
      map (\entityId ->
        { entity: wrapEntity entityId
        , components: readComponents (Proxy :: Proxy rl) archId entityId world'
        }
      ) arch.entities
```

- [ ] **Step 3: Run tests**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass. Query semantics are unchanged.

- [ ] **Step 4: Re-run benchmarks**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-task6.txt
```

Expected: another perceptible drop in `query+update` time, especially at higher A (archetype count). If A=1 (all entities in same archetype, the bench case), the win is small here — that's fine; the next task hits the per-entity path.

- [ ] **Step 5: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/Query.purs bench-after-task6.txt
git commit -m "perf: thread archId through runQuery, replace quadratic <> [x]

runQuery previously reverse-looked-up archId from arch.mask via a full
linear scan (O(A^2) per query). Thread the (archId, arch) pair from
the iteration directly. Also replace four occurrences of foldl + <> [x]
with filter/mapMaybe/concatMap (single-pass, single-alloc)."
```

---

## Task 7: Hoist archetype/storage lookups out of the per-entity-per-component loop

`readComponentValue` (Query.purs lines 404-435) is called once per component per entity from `readComponents`. Each call does:
- `Map.lookup archId world.archetypes` — O(log A)
- `Map.lookup entityIndex arch.entityPositions` — O(log N)
- `CS.lookup label arch.storage` — O(1) but allocates a `Just`

For a 4-component query over 200 entities, that's 800 redundant Map traversals. The archetype, entity position, and storage handles should be resolved **once per archetype per entity** (or even once per archetype, then indexed by entity row).

The cleanest fix: change the `ReadComponents` typeclass signature so the resolved `Archetype` and the entity's position are passed through, instead of `(ArchetypeId, EntityId, World)` requiring re-resolution per component.

**Files:**
- Modify: `src/ECS/Query.purs` (rewrite `ReadComponents` class and instances; update both `extractFromArchetype` and `extractFromArchetypeCached` to resolve once per entity)

- [ ] **Step 1: Rewrite `ReadComponents` class signature**

Replace the existing class definition and instances (Query.purs lines 367-435) with:

```purescript
-- | Read components from a resolved archetype + row index into a typed record.
-- |
-- | The caller must have already resolved the archetype and the entity's row
-- | position. The class then walks the RowList and indexes each component
-- | column at that fixed row, allocating only the result record.
class ReadComponents (rl :: RowList Type) (r :: Row Type) | rl -> r where
  readComponents :: Proxy rl -> Archetype -> Int -> Record r

instance readComponentsNil :: ReadComponents RL.Nil () where
  readComponents _ _ _ = {}

instance readComponentsCons ::
  ( IsSymbol label
  , Cons label typ r' r
  , Lacks label r'
  , ReadComponents tail r'
  ) =>
  ReadComponents (RL.Cons label typ tail) r where
  readComponents _ arch rowIdx =
    let
      labelStr       = reflectSymbol (Proxy :: Proxy label)
      componentValue = readColumnAt labelStr arch rowIdx
      rest           = readComponents (Proxy :: Proxy tail) arch rowIdx
    in
      Record.insert (Proxy :: Proxy label) componentValue rest

-- | Read one component value from a resolved archetype at a known row index.
-- |
-- | This is the lowest-level read in the hot path. It does one CS.lookup
-- | (Foreign-Object hash lookup) and one Array.index. No Map traversals.
readColumnAt :: forall a. String -> Archetype -> Int -> a
readColumnAt label arch rowIdx =
  case CS.lookup label arch.storage of
    Nothing  -> CS.componentFromForeign (CS.componentToForeign unit)
    Just col -> case CS.arrayIndex rowIdx col of
      Nothing -> CS.componentFromForeign (CS.componentToForeign unit)
      Just fv -> CS.componentFromForeign fv
```

You can now delete the old `readComponentValue` function (it's superseded). The `ArchetypeId` and `EntityId` parameters are gone from the class.

- [ ] **Step 2: Update both `extractFromArchetype` callers to pass `Archetype + rowIdx`**

In `runQuery`'s `extractFromArchetype` (the one rewritten in Task 6), change:

```purescript
    extractFromArchetype :: ArchetypeId -> Archetype -> World -> Array (QueryResult required)
    extractFromArchetype archId arch world' =
      map (\entityId ->
        { entity: wrapEntity entityId
        , components: readComponents (Proxy :: Proxy rl) archId entityId world'
        }
      ) arch.entities
```

to:

```purescript
    extractFromArchetype :: Archetype -> Array (QueryResult required)
    extractFromArchetype arch =
      Array.mapWithIndex
        (\rowIdx entityId ->
            { entity: wrapEntity entityId
            , components: readComponents (Proxy :: Proxy rl) arch rowIdx
            })
        arch.entities
```

The caller in `runQuery` body becomes:

```purescript
        in
          Array.concatMap
            (\(Tuple _ arch) -> extractFromArchetype arch)
            matches
```

(`archId` is now unused at the extract level.)

Similarly in `runQueryCached`'s `extractFromArchetypeCached`:

```purescript
    extractFromArchetypeCached :: Archetype -> Array (QueryResult required)
    extractFromArchetypeCached arch =
      Array.mapWithIndex
        (\rowIdx entityId ->
            { entity: wrapEntity entityId
            , components: readComponents (Proxy :: Proxy rl) arch rowIdx
            })
        arch.entities
```

And the caller:

```purescript
          results = Array.concatMap
            (\archId -> case Map.lookup archId worldAfterCache.archetypes of
                Nothing   -> []
                Just arch -> extractFromArchetypeCached arch)
            matchingArchIds
```

Note the cached path still does **one** `Map.lookup` per matching archetype (not per entity). That's correct — A is small.

- [ ] **Step 3: Update the export list and any unused imports**

`runQuery`'s `where`-block no longer uses `World` for `extractFromArchetype`. Remove unused parameters. Ensure `Array.mapWithIndex` is imported via `import Data.Array as Array`. Remove the `import ECS.Entity (entityIndex)` if it's no longer used in this module — but check first; `runQuery` itself may still use it (it doesn't, after these changes; only `entityIndex` was used in old `readComponentValue`).

- [ ] **Step 4: Tests**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass. The semantic of reading components is unchanged.

- [ ] **Step 5: Re-run benchmarks**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-task7.txt
```

Expected: `query+update` improves further — the per-entity Map.lookup chain is gone. With 1000 entities × 2 components in the bench, that's ~2000 fewer Map.lookups per query.

- [ ] **Step 6: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/Query.purs bench-after-task7.txt
git commit -m "perf: hoist archetype/position lookups out of per-entity loops

readComponentValue did Map.lookup archId, Map.lookup entityIdx, and
CS.lookup label per (entity, component). For a C-component query over
N entities that is N*C*(log A + log N) Map traversals. Resolve
archetype + row index once per entity in the iterating extract step,
then index the columns by row at the typeclass leaves."
```

---

## Task 8: Re-key archetypes by `ComponentMask :: Int`

Today `Map ArchetypeId Archetype` is `Map String Archetype`. Every add/remove computes the new ID via `parseArchetypeId` (split on ",") + `sort` + `joinWith`. The mask is already computed in parallel. Just key by the mask: `Map ComponentMask Archetype` (i.e. `Map Int Archetype`). Keep the string ID for debugging/serialisation — produce it lazily, but don't put it on the hot path.

This is a wider refactor — touching `World`, `Component`, `Query`. Do it as one cohesive change.

**Files:**
- Modify: `src/ECS/World.purs` (change `archetypes` field type, `entityLocations` value type, helper signatures)
- Modify: `src/ECS/Component.purs` (replace `addLabelToArchetype`/`removeLabelFromArchetype` string manipulation with mask updates; archetype lookup by new mask)
- Modify: `src/ECS/Query.purs` (uses `ArchetypeId` only as a key; pass mask through)
- Modify: tests if they reference `ArchetypeId` strings (unlikely — most tests check observable behaviour)

- [ ] **Step 1: Change `ArchetypeId` and World fields**

In `/home/toby/pure-ecs/src/ECS/World.purs`:

Replace the `ArchetypeId` type alias (line 78):

```purescript
-- | Archetype ID is the bitmask of component bits.
-- | Two archetypes are the same iff they have the same set of components,
-- | which the bitmask captures exactly. Keys in `world.archetypes` are masks.
type ArchetypeId = ComponentMask
```

Update the World record (lines 103-110):

```purescript
type World =
  { entities          :: EntityManager
  , archetypes        :: Map ArchetypeId Archetype
  , entityLocations   :: Map Int ArchetypeId
  , componentRegistry :: ComponentRegistry
  , structuralVersion :: Int
  , queryCache        :: Map QueryCacheKey CachedQueryResult
  }
```

(The signature is the same — `ArchetypeId` just resolves to `Int` now.)

Update `emptyArchetypeId` (line 147):

```purescript
emptyArchetypeId :: ArchetypeId
emptyArchetypeId = 0
```

And `getOrCreateEmptyArchetype` (line 360-363) — it already uses the mask `0` and `Set.empty`. Just confirm it still type-checks under `ArchetypeId = Int`.

- [ ] **Step 2: Replace `addLabelToArchetype`/`removeLabelFromArchetype`**

In `/home/toby/pure-ecs/src/ECS/Component.purs`:

Delete the string-manipulation helpers `parseArchetypeId`, `buildArchetypeId`, `addLabelToArchetype`, `removeLabelFromArchetype`, `archetypeContains` (lines 646-695). Also delete `archetypeContains` and the unused `range`/`(..)` helpers (lines 585-590).

Remove the now-unused imports at the top of the file:

```purescript
import Data.Array (filter, findIndex, sort, take, updateAt, (:))
import Data.String (Pattern(..), joinWith, split)
import Data.String.Common (trim)
```

becomes:

```purescript
import Data.Array (index, length, take, updateAt)
```

(Verify `index`, `length`, `take`, `updateAt` are still used in this file. They are — `removeFromArchetype` uses them.)

Now replace every callsite that previously computed a string archetype ID with one that uses the mask:

- `addComponentPure` body — currently `newArchId = addLabelToArchetype labelStr oldArchId`. Replace with computing the new mask:

```purescript
        Just oldArchId ->
          let
            { bit: labelBit, world: worldWithBit } = getOrCreateComponentMask labelStr world
            oldMask = case Map.lookup oldArchId worldWithBit.archetypes of
              Nothing   -> oldArchId  -- treat archId itself as the old mask (the empty/zero case)
              Just arch -> arch.mask
            newArchId = maskAddBit oldMask labelBit
            result = moveEntityWithComponent entityId oldArchId newArchId labelStr labelBit (CS.componentToForeign componentValue) worldWithBit
          in
            { world: result.world, entity: wrapEntity entityId }
```

(See Step 4 for the new `moveEntityWithComponent` signature.)

- `removeComponentPure` body — similarly:

```purescript
        Just oldArchId ->
          let
            removedBit = case Map.lookup labelStr world.componentRegistry.labelToBit of
              Just b  -> b
              Nothing -> 0
            oldMask = case Map.lookup oldArchId world.archetypes of
              Nothing   -> oldArchId
              Just arch -> arch.mask
            newArchId = maskRemoveBit oldMask removedBit
            result = moveEntityToArchetype entityId oldArchId newArchId labelStr world
          in
            { world: result.world, entity: wrapEntity entityId }
```

- `hasComponent` (line 360) — remove the `archetypeContains` fallback (it's now incoherent with `ArchetypeId = Int`). The cached `arch.labels` Set is the source of truth:

```purescript
hasComponent labelProxy entity world =
  let
    labelStr = reflectSymbol labelProxy
    entityId = unEntity entity
    idx      = entityIndex entityId
  in
    case Map.lookup idx world.entityLocations of
      Nothing -> false
      Just archId -> case Map.lookup archId world.archetypes of
        Nothing   -> false
        Just arch -> Set.member labelStr arch.labels
```

- [ ] **Step 3: Compute archetype labels from the source archetype, not from the ID**

The two existing places that re-derive labels via `Set.fromFoldable $ parseArchetypeId newArchId` (Component.purs lines 446 and 476/611) can no longer parse a string. Compute labels directly from the source archetype's labels set:

In `moveEntityToArchetype` (around line 444-446):

```purescript
    -- New archetype's labels = source labels minus the removed one (no string parsing)
    sourceLabels = case Map.lookup oldArchId world.archetypes of
      Just arch -> arch.labels
      Nothing   -> Set.empty
    newArchLabels = Set.delete removedLabel sourceLabels
```

In `addToArchetypeWithAllComponents` (around line 470-477) and `addToArchetypeWithComponent` (around line 605-612), the new archetype's labels are computed by the caller and passed in. Refactor: change these helpers' signatures to take `newArchLabels :: Set String` directly.

`addToArchetypeWithAllComponents` becomes:

```purescript
addToArchetypeWithAllComponents
  :: EntityId -> ArchetypeId -> ComponentMask -> Set String
  -> ComponentStorage -> World -> World
addToArchetypeWithAllComponents entityId archId newMask newLabels allComponents world =
  let
    isNewArchetype = not $ Map.member archId world.archetypes

    arch = case Map.lookup archId world.archetypes of
      Just a -> a
      Nothing ->
        { entities: []
        , entityPositions: Map.empty
        , mask: newMask
        , labels: newLabels
        , storage: CS.empty
        }
    -- ... rest of the body unchanged
```

`addToArchetypeWithComponent` becomes:

```purescript
addToArchetypeWithComponent
  :: EntityId -> ArchetypeId -> ComponentMask -> Set String
  -> ComponentStorage -> String -> Foreign -> World -> World
addToArchetypeWithComponent entityId archId newMask newLabels existingComponents newLabel newComponentValue world =
  -- same as before but uses newLabels in the Nothing branch
```

`moveEntityWithComponent` and `moveEntityToArchetype` need to compute and pass these label sets. For `moveEntityWithComponent`, the new label set is `Set.insert newLabel sourceLabels`; for `moveEntityToArchetype`, it's `Set.delete removedLabel sourceLabels`.

`moveEntityWithComponent` signature changes — accept `newLabel :: String` and `labelBit :: Int` already; compute new labels inside:

```purescript
moveEntityWithComponent
  :: EntityId -> ArchetypeId -> ArchetypeId -> String -> Int -> Foreign -> World
  -> { world :: World }
moveEntityWithComponent entityId oldArchId newArchId label labelBit componentValue world =
  let
    idx = entityIndex entityId

    sourceArch = Map.lookup oldArchId world.archetypes
    sourceLabels = case sourceArch of
      Just a  -> a.labels
      Nothing -> Set.empty
    newLabels = Set.insert label sourceLabels

    oldMask = case sourceArch of
      Nothing -> 0
      Just a  -> a.mask
    newMask = maskAddBit oldMask labelBit

    existingComponents = case sourceArch of
      Nothing -> CS.empty
      Just arch -> case Map.lookup idx arch.entityPositions of
        Nothing       -> CS.empty
        Just entityIdx -> extractAllComponentsForEntity entityIdx arch.storage

    world1 = removeFromArchetype entityId oldArchId world
    world2 = addToArchetypeWithComponent entityId newArchId newMask newLabels existingComponents label componentValue world1

    updatedLocations = Map.insert idx newArchId world2.entityLocations
  in
    { world: world2 { entityLocations = updatedLocations } }
```

(Adjust `addComponentPure`'s call site to pass `labelBit` — already computed via `getOrCreateComponentMask`.)

- [ ] **Step 4: `Query.purs` — `ArchetypeId` is now `Int`**

Query.purs uses `ArchetypeId` only as a key in `world.archetypes` and in the cache. With `ArchetypeId = Int`, no source changes are needed in Query.purs aside from the imports — but verify nothing references string-keyed archetype IDs.

- [ ] **Step 5: Tests**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass. Some tests may have been inspecting `entityLocations`'s value or `world.archetypes`'s keys as strings — if so, update them to use Int.

If a test inspects `world.archetypes` keys directly, it should now compare against the bitmask. The test in `WorldSpec.purs` may have an `entityLocations`-as-string assumption — fix it to compare against the Int mask.

- [ ] **Step 6: Re-run benchmarks**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-task8.txt
```

Expected: `spawn 1000` improves substantially — every spawn previously paid string-build + Map(String) lookup; now it's pure Int. `query+update` improves marginally (its Map.lookups are now Int-keyed instead of String-keyed).

- [ ] **Step 7: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/World.purs src/ECS/Component.purs src/ECS/Query.purs test/ bench-after-task8.txt
git commit -m "perf: key archetypes by ComponentMask (Int) instead of String

ArchetypeId is now ComponentMask (= Int). Eliminates parseArchetypeId,
buildArchetypeId, sort, joinWith on every component add/remove.
Archetype Map lookups become integer compares. Cached label sets are
derived from the source archetype's labels, not parsed from the ID."
```

---

## Task 9: Re-key query cache by mask tuple

`makeQueryCacheKey` builds a fresh string `"req:N,exc:M"` on every cache hit. The key is just two ints — use them directly.

**Files:**
- Modify: `src/ECS/World.purs` (change `QueryCacheKey` type and `makeQueryCacheKey`)
- Modify: `src/ECS/Query.purs` (usage is mostly unchanged — the key constructor is opaque)

- [ ] **Step 1: Change `QueryCacheKey` type**

In `/home/toby/pure-ecs/src/ECS/World.purs`:

Replace `type QueryCacheKey = String` (line 83) with:

```purescript
type QueryCacheKey = Tuple ComponentMask ComponentMask
```

(`Tuple` already imported.)

Replace `makeQueryCacheKey` (line 432-434):

```purescript
makeQueryCacheKey :: ComponentMask -> ComponentMask -> QueryCacheKey
makeQueryCacheKey requiredMask excludedMask = Tuple requiredMask excludedMask
```

- [ ] **Step 2: Verify Query.purs still compiles**

`runQueryCached` uses the key opaquely: `let cacheKey = makeQueryCacheKey requiredMask excludedMask`, then `Map.lookup cacheKey w.queryCache`, then `Map.insert cacheKey ... world.queryCache`. This works with any `Ord` key. `Tuple ComponentMask ComponentMask` is `Ord`. No source changes needed.

- [ ] **Step 3: Tests**

```bash
cd /home/toby/pure-ecs
spago test 2>&1 | tail -40
```

Expected: all tests pass.

- [ ] **Step 4: Bench**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-task9.txt
```

Expected: minor improvement on hot query paths — string-build is gone from every cache lookup.

- [ ] **Step 5: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/World.purs bench-after-task9.txt
git commit -m "perf: key query cache by (Int, Int) tuple instead of formatted string

makeQueryCacheKey previously allocated 'req:N,exc:M' on every call.
Use the masks directly as the Map key — Tuple Int Int is Ord."
```

---

## Task 10: Add a perf scenario + per-system timing overlay to WebDemo

The shipped `WebDemo.purs` only spawns 4 entities, so users can't hit the perf cliff that motivated this work. Add a "stress" mode that spawns 1000+ entities and shows per-tick timing.

**Files:**
- Modify: `src/ECS/Examples/WebDemo.purs` (entity count config, FFI for `performance.now()`, overlay)

- [ ] **Step 1: Read the current WebDemo to see where entity count and tick happen**

```bash
cd /home/toby/pure-ecs
sed -n '180,260p' src/ECS/Examples/WebDemo.purs
```

Identify the entity-spawn block and the tick function. Confirm the structure before editing.

- [ ] **Step 2: Bump the entity count and add per-tick timing**

This step is more open-ended than the others — the diff depends on WebDemo.purs's current shape. The principle: use FFI to call `performance.now()` before and after each `gameTick`, and surface the delta either in a debug DOM element or via `Effect.Console.log`.

Minimal FFI in a new file `src/ECS/Examples/WebDemo.js` (PureScript foreign import target) — only needed if `performance.now` isn't already imported from `purescript-now`:

```javascript
export const performanceNow = () => performance.now();
```

In `WebDemo.purs`, declare `foreign import performanceNow :: Effect Number` and wrap the tick:

```purescript
t0 <- performanceNow
let { world: w', result: _ } = runSystem combinedSystem world
t1 <- performanceNow
log $ "tick: " <> show (t1 - t0) <> " ms (" <> show entityCount <> " entities)"
```

Bump the entity count from 4 to a configurable constant `entityCount = 1000` (or higher).

- [ ] **Step 3: Build and verify the demo still runs**

```bash
cd /home/toby/pure-ecs
npm run build:demo
npm run serve &
# Open http://localhost:8000 and confirm: 1000 entities render, console logs tick times
```

Expected: tick time under ~5 ms after all preceding fixes. If it's more than 50 ms, something regressed — go back and re-bench.

- [ ] **Step 4: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/Examples/WebDemo.purs src/ECS/Examples/WebDemo.js docs/app.js
git commit -m "demo: add 1000-entity stress mode and per-tick timing overlay

Default entity count was 4 — far below the cliff users hit. Stress
mode spawns 1000 + logs ms per gameTick to console so users can
reproduce and verify perf claims."
```

---

## Task 11: Update CLAUDE.md performance characteristics table

The performance claims in `CLAUDE.md` and `src/ECS/CLAUDE.md` were inaccurate before this work and (now) inaccurate in the other direction. Update the table to reflect actual measured complexity.

**Files:**
- Modify: `/home/toby/pure-ecs/CLAUDE.md` (the "Performance Characteristics" table)
- Modify: `/home/toby/pure-ecs/src/ECS/CLAUDE.md` (same table)

- [ ] **Step 1: Update the table**

In both `CLAUDE.md` files, replace the "Performance Characteristics" table with:

```markdown
## Performance Characteristics

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| spawnEntity | O(log A) | O(1) | Entity recycled via free list, archetype map lookup |
| addComponent | O(log A + N) | O(N) | A=archetypes; N=column copy on archetype migration |
| removeComponent | O(log A + N) | O(N) | Archetype migration |
| **setComponent (in-place value update)** | **O(log A + log N)** | **O(1)** | **Single column write — no migration** |
| getComponent | O(log A + log N) | O(1) | Map.lookup × 2 + Array.index |
| query (cached) | O(N + C·N) | O(N) | C = required component count; one column read per (entity, component) |
| query (uncached) | O(A + N + C·N) | O(N) | First call also computes archetype filter |
| runSystem | depends on body | depends | Composes via State monad |

Where: A = number of archetypes, N = entities in the matching archetypes, C = number of required components.

**Hot-path principle:** `updateComponent`/`modifyComponent` use `setComponent` internally — value writes never trigger archetype migration. Only `addComponent`/`removeComponent` (which change the row type) pay migration cost.
```

- [ ] **Step 2: Commit**

```bash
cd /home/toby/pure-ecs
git add CLAUDE.md src/ECS/CLAUDE.md
git commit -m "docs: update performance characteristics to match implementation

The previous table claimed O(1) swap-remove (false: O(N) array copy
in immutable storage) and didn't distinguish setComponent's in-place
write from add/removeComponent's migration cost. Updated with measured
complexity and the hot-path principle."
```

---

## Final verification

- [ ] **Step 1: Run the full test suite**

```bash
cd /home/toby/pure-ecs
npm run clean
spago build
spago test
```

Expected: all tests pass.

- [ ] **Step 2: Run the bench suite a final time and compare against baseline**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-final.txt
diff bench-baseline.txt bench-final.txt
```

Expected: every scenario significantly faster than the baseline. Document the speedups in the final commit/PR.

- [ ] **Step 3: Verify the demo**

```bash
cd /home/toby/pure-ecs
npm run build:demo
npm run serve &
# Open http://localhost:8000, confirm 1000-entity stress mode runs at >30 fps
```

- [ ] **Step 4: Final commit summarising the work (optional, only if there's a meaningful summary doc to add)**

If desired, add a `PERF_REPORT.md` at the repo root summarising baseline → final ratios per scenario. Otherwise, the per-task commits already tell the story.

---

## Out of scope (deliberate)

These were identified in the multi-agent review but are explicitly **not** in this plan:

1. **Mutable typed column buffers (`STArray`, `Float64Array`).** This is the path to 100k+ entities at 60fps. It requires a fundamental rewrite of the storage layer behind a `runST` boundary. Worth its own plan once the gains in this one are measured.
2. **Dense `Array EntityLocation` keyed by entity index.** A useful follow-up but interacts with entity ID recycling and per-archetype position tracking. Diminishing returns relative to Tasks 4–9.
3. **Streaming queries (`forEachQuery_` callback API).** Currently `queryFor` materialises a full array of QueryResult records. Streaming would skip that allocation entirely. Worth doing if benchmarks after this plan still show GC pressure on the inner loop.

These are listed here so future-you (or a follow-up plan) has a clear next step.
