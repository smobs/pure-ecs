# Profiler-Audit Follow-up Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the *corrected* recommendations from `PROFILER_AUDIT_2026-06-16.md`, after a five-agent review found the audit factually accurate (16/16 cited locations land, 0 wrong) but identified one outright-wrong recommendation, a measurement-rigor gap, and one bug the audit missed.

**Architecture:** Land the cheap, low-risk, no-API-change wins now (Win A + the S5 cache bug + closing the S1 silent-corruption hole), then build the *measurement gate* the audit skipped, and only *then* decide on the two expensive items (P3 cache re-keying, Win B column-iteration API) based on numbers from the bench harness that already lives in this repo.

**Tech Stack:** PureScript 0.15.x, spago (next), `purs-backend-es` (already configured), `purescript-spec` for tests, `Effect.Now` bench harness in `bench/`.

---

## Why this plan differs from the audit (the review's corrections)

The audit drives the work below, with three corrections the review insisted on:

1. **Win A option #1 is wrong — dropped.** The audit offered two ways to stop rebuilding the query mask on every cached call: (a) cache the masks *on the `Query` value* keyed by `nextBit`, or (b) a `World`-level resolution cache. Option (a) is a **no-op**: in the `queryFor @row` idiom that dominates, the `Query` value is reconstructed fresh on every call (`System.purs:127-129`, `Query.purs:84-86`), so a field cached on it is never read on the next call. **This plan implements only (b).** (Verified against compiled `output-es/` by the PureScript reviewer.)

2. **The audit's percentages are ordinal, not additive.** P1 (35-45%) + P2 (30-40%) + P3 (10-15%) ≈ 100% of the library's 6.6% slice, leaving ~nothing for iteration/GC/State plumbing — implausible as cardinal numbers, and derived from one consumer's 15-second Firefox profile that was never reproduced in this repo's bench harness. Treat them as "P1 and P2 dominate," and **substantiate every perf claim with a committed bench delta** (the discipline the prior `2026-04-29-perf-overhaul.md` plan established and this one continues).

3. **Win B is the prior plan's deferred item, and it stays gated.** The audit's "Win B" (column-iteration query API) is verbatim the `2026-04-29` plan's out-of-scope item #3 ("Streaming queries… worth doing **if** benchmarks after this plan still show GC pressure"). The audit silently dropped that precondition. This plan **reinstates the gate**: Win B is implemented only if a post-Win-A bench (Task 6) shows wide-record materialization is still a material fraction of a read-heavy query loop.

New bug found by the review (not in the audit): **S5** — `spawnEntityPure` is the one archetype-*creating* path that omits the `structuralVersion` bump, so a query cached against an empty world can go stale across the first spawn (Task 2).

S1's fix is **decoupled** from Win B (the audit folded them together): the silent `unsafeCoerce unit` fallback is closed cheaply and independently in Task 5.

---

## File Structure

**Modified (do-now tasks 1-6):**
- `src/ECS/World.purs` — add `maskCache` field + `resolveQueryMasks` (Win A part 1); bump `structuralVersion` in `spawnEntityPure` on first empty-archetype creation (S5).
- `src/ECS/Query.purs` — route `runQuery`/`runQueryCached` through `World.resolveQueryMasks`; delete the per-call label-fold helpers; single-`reflectSymbol` via `unsafeSet` (Win A part 2); replace `readColumnAt`'s fabrication arms with `unsafeCrashWith` (S1).
- `test/QuerySpec.purs` — regression tests for S5 and the Win-A mask-cache `nextBit` guard.
- `bench/src/ECS/Bench.purs` + `bench/src/Main.purs` — add the read-heavy gating scenario (Task 6).

**Gated (tasks 7-8, conditional on measurements):**
- P3: `src/ECS/World.purs` + `src/ECS/Query.purs` — per-signature cache invalidation.
- Win B: new column-iteration query entry point in `src/ECS/Query.purs`.

**Docs (task 9):**
- `PROFILER_AUDIT_2026-06-16.md` — append a short "Corrections" addendum so the record is honest.
- `CLAUDE.md` + `src/ECS/CLAUDE.md` — note the mask cache and the S5/S1 fixes.

---

## Conventions

- One commit per task. Conventional Commits: `feat:`/`fix:`/`perf:`/`bench:`/`docs:`/`test:`.
- **Use `npm test`, NOT `spago test`.** Per `src/ECS/CLAUDE.md`, `spago test` is broken under `purs-backend-es`; `npm test` does `spago build` then runs the bundled test main on node.
- After every code change, run `npm test` and don't proceed until green.
- Every perf task commits its bench delta (the harness exists to substantiate claims).
- End every commit message body with:
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`

---

## Task 1: Establish a green baseline + capture pre-change bench

We need a known-good build and current numbers before touching anything, so Win A's delta is measured against the same machine/run.

**Files:** none modified.

- [ ] **Step 1: Confirm tests are green**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -20
```

Expected: all specs pass (145+ tests; the suite registers entity/world/component/query/system/integration/pipeline/docs/write/debug specs — see `test/Main.purs`).

- [ ] **Step 2: Capture the current bench numbers**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-pre-followup.txt
```

Expected: three lines (spawn 1k, query+update 1k, 10 ticks 1k) with `ms (avg of 5, sink=…)`. These are the baseline Win A is measured against.

- [ ] **Step 3: Commit the baseline snapshot**

```bash
cd /home/toby/pure-ecs
git add bench-pre-followup.txt
git commit -m "bench: capture pre-followup baseline before audit-driven changes

Snapshot of spawn/query/tick scenarios at HEAD before Win A / S5 / S1
work, so subsequent perf deltas are measured against the same run."
```

---

## Task 2: Fix S5 — `spawnEntityPure` must bump `structuralVersion` when it creates the empty archetype

The review found that `spawnEntityPure` (`World.purs:239-266`) is the only archetype-*creating* code path that omits `incrementStructuralVersion`. So a query cached against a world with zero archetypes (e.g. `query @()` against `emptyWorld`) stays "valid" across the first spawn and returns a stale `[]`. Every other creator (`Component.purs:531`, `:669`) already guards the bump with `isNewArchetype`; mirror that.

**Files:**
- Test: `test/QuerySpec.purs` (add a test after the existing cache test, which ends at line 605)
- Modify: `src/ECS/World.purs:239-266` (`spawnEntityPure`)

- [ ] **Step 1: Write the failing test**

In `/home/toby/pure-ecs/test/QuerySpec.purs`, immediately after the `it "cached query sees a new high-bit archetype after structural change"` block (it ends at line 605, just before `-- Edge Cases`), add inside the same `describe "Basic Queries"` block:

```purescript
      it "cached empty-row query is not stale after the first spawn (S5)" do
        -- spawnEntityPure creates the empty archetype on the first spawn. If
        -- it omits the structuralVersion bump, a query cached against a world
        -- with zero archetypes stays valid and misses the spawned entity.
        let q :: _ () ()
            q = query (Proxy :: _ ())
            -- Cache the empty-row query against a world with NO archetypes.
            r1 = runQueryCached q emptyWorld
            -- First spawn creates the empty archetype (the only absent->present
            -- transition that previously skipped the version bump).
            w1 = (spawnEntityPure r1.world).world
            -- Second call must see the spawned entity, not a stale [].
            r2 = runQueryCached q w1
        length r1.results `shouldEqual` 0
        length r2.results `shouldEqual` 1
```

(All identifiers used — `query`, `runQueryCached`, `emptyWorld`, `spawnEntityPure`, `length`, `Proxy` — are already imported in `QuerySpec.purs`.)

- [ ] **Step 2: Run the test to verify it fails**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | grep -A3 "S5"
```

Expected: FAIL — `r2.results` has length 0 (stale cache), expected 1.

- [ ] **Step 3: Fix `spawnEntityPure`**

In `/home/toby/pure-ecs/src/ECS/World.purs`, replace the body of `spawnEntityPure` (lines 239-266) with:

```purescript
spawnEntityPure :: World -> { world :: World, entity :: Entity () }
spawnEntityPure world =
  let
    -- Step 1: Create EntityId (see CLAUDE.md State monad pattern)
    (Tuple entityId state) = runState createEntity world.entities

    -- S5 fix: creating the empty archetype is a structural change. If it's
    -- absent (fresh world / first spawn), the bump invalidates query caches
    -- built before any archetype existed. After the first spawn the empty
    -- archetype persists (despawn never deletes it), so this is a one-time
    -- cost, not a per-spawn cost.
    isNewArchetype = not (Map.member emptyArchetypeId world.archetypes)

    -- Step 2: Get or create empty archetype
    emptyArch = getOrCreateEmptyArchetype world.archetypes

    -- Step 3: Add entity to empty archetype with position tracking
    newPosition = length emptyArch.entities
    updatedArch = emptyArch
      { entities = emptyArch.entities <> [entityId]
      , entityPositions = Map.insert (entityIndex entityId) newPosition emptyArch.entityPositions
      }
    updatedArchetypes = Map.insert emptyArchetypeId updatedArch world.archetypes

    -- Step 4: Update entity locations
    updatedLocations = Map.insert (entityIndex entityId) emptyArchetypeId world.entityLocations

    -- Step 5: Build updated world (bump structuralVersion iff we just created
    -- the empty archetype)
    baseWorld = world
      { entities = state
      , archetypes = updatedArchetypes
      , entityLocations = updatedLocations
      }
    newWorld =
      if isNewArchetype
        then incrementStructuralVersion baseWorld
        else baseWorld
  in
    { world: newWorld, entity: Entity entityId }
```

(`Map.member`, `incrementStructuralVersion`, and `emptyArchetypeId` are all already in scope in `World.purs`.)

- [ ] **Step 4: Run the test to verify it passes**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -20
```

Expected: all tests pass, including the new S5 test.

- [ ] **Step 5: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/World.purs test/QuerySpec.purs
git commit -m "fix: bump structuralVersion when spawnEntityPure creates the empty archetype (S5)

spawnEntityPure was the only archetype-creating path that skipped the
structuralVersion bump, so a query cached against a world with zero
archetypes (e.g. query @() on emptyWorld) stayed valid across the first
spawn and returned a stale []. Guard the bump with isNewArchetype, the
same pattern Component.purs already uses; after the first spawn the empty
archetype persists, so there is no per-spawn cost. Found by the
2026-06-16 audit review (not in the audit itself).

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 3: Win A part 1 — `World`-level mask-resolution cache

`runQueryCached` resolves the query's required/excluded label sets to bitmasks on **every** call, *before* consulting the query cache (`Query.purs:202-203`, `:210`, `:213`) — a `Map.lookup` per label against the registry, even on a cache hit (P1). A label's bit never changes once assigned and the registry only grows, so the resolution is stable until `componentRegistry.nextBit` increases. Memoise it on the `World`, keyed by the `(required, excluded)` label sets and guarded by `nextBit`.

**Files:**
- Modify: `src/ECS/World.purs` (add types, `maskCache` field, `resolveQueryMasks`; export it; update `emptyWorld`; add `foldl` import)
- Modify: `src/ECS/Query.purs` (route both `runQuery` and `runQueryCached` through `resolveQueryMasks`; delete the local fold helpers; clean up imports)
- Test: `test/QuerySpec.purs` (mask-cache `nextBit`-guard regression test)

- [ ] **Step 1: Write the regression test (guards correctness of the new cache)**

This is a perf change with no intended behaviour change, so the test guards the one bug the cache could introduce: a stale `Nothing` resolution surviving the registration of a previously-unknown label. It passes on current code (which re-resolves every time) and must keep passing after the cache lands.

In `/home/toby/pure-ecs/test/QuerySpec.purs`, after the S5 test added in Task 2, add:

```purescript
      it "mask-resolution cache re-resolves when a new component registers (Win A)" do
        -- runQueryCached memoises label->mask resolution, guarded by the
        -- registry's nextBit. A query for an unregistered label resolves to
        -- Nothing (empty results) and caches that. After the label registers
        -- (nextBit grows), the SAME query must re-resolve and find the entity.
        let q :: _ (mana :: Int) ()
            q = query (Proxy :: _ (mana :: Int))
            -- First call: 'mana' unregistered -> Nothing -> [] (caches the
            -- resolution at the current nextBit).
            r1 = runQueryCached q emptyWorld
            -- Spawn an entity carrying 'mana' (registers it, grows nextBit).
            w1 = execState (void $ spawnEntity <+> (Proxy :: _ "mana") := 5) r1.world
            -- Second call must re-resolve and see the entity.
            r2 = runQueryCached q w1
        length r1.results `shouldEqual` 0
        length r2.results `shouldEqual` 1
```

(All identifiers — `execState`, `void`, `spawnEntity`, `<+>`, `:=` — are already imported in `QuerySpec.purs`.)

- [ ] **Step 2: Run it to confirm it passes on current code**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | grep -A3 "Win A"
```

Expected: PASS (current code re-resolves on every call, so there's no staleness). This establishes the invariant the cache must preserve.

- [ ] **Step 3: Add the mask-cache types and field to `World.purs`**

In `/home/toby/pure-ecs/src/ECS/World.purs`:

(3a) Add `foldl` to the `Data.Foldable` import. Change line 51 from:

```purescript
import Data.Foldable (sum)
```

to:

```purescript
import Data.Foldable (foldl, sum)
```

(3b) Add the new types to the export list. Change the export block (lines 19-20) from:

```purescript
  , QueryCacheKey
  , CachedQueryResult
```

to:

```purescript
  , QueryCacheKey
  , CachedQueryResult
  , MaskCacheKey
  , MaskCacheEntry
```

and add `resolveQueryMasks` to the "Query cache helpers" export section (after `incrementStructuralVersion`, line 40):

```purescript
  -- Query cache helpers
  , makeQueryCacheKey
  , incrementStructuralVersion
  , resolveQueryMasks
```

(3c) After the `CachedQueryResult` type (line 124), add:

```purescript
-- | Key for the mask-resolution cache: the query's (required, excluded)
-- | label sets. These are what's available *before* resolving masks, so
-- | the cache can be consulted without paying the per-label registry folds.
type MaskCacheKey = Tuple (Set String) (Set String)

-- | A memoised label-set -> bitmask resolution.
-- |
-- | `required` is `Nothing` when some required label was unregistered at
-- | resolution time. `nextBit` records the registry size when this entry was
-- | computed: a label's bit never changes once assigned and the registry only
-- | grows, so the entry is valid iff `nextBit` still equals the registry's
-- | current `nextBit`.
type MaskCacheEntry =
  { required :: Maybe ComponentMask
  , excluded :: ComponentMask
  , nextBit  :: Int
  }
```

(3d) Add `maskCache` to the `World` record. Change lines 135-142 from:

```purescript
type World =
  { entities :: EntityManager
  , archetypes :: Map ArchetypeId Archetype
  , entityLocations :: Map Int ArchetypeId
  , componentRegistry :: ComponentRegistry
  , structuralVersion :: Int
  , queryCache :: Map QueryCacheKey CachedQueryResult
  }
```

to:

```purescript
type World =
  { entities :: EntityManager
  , archetypes :: Map ArchetypeId Archetype
  , entityLocations :: Map Int ArchetypeId
  , componentRegistry :: ComponentRegistry
  , structuralVersion :: Int
  , queryCache :: Map QueryCacheKey CachedQueryResult
  , maskCache :: Map MaskCacheKey MaskCacheEntry
  }
```

(3e) Initialise it in `emptyWorld`. Change lines 202-210 from:

```purescript
emptyWorld =
  { entities: emptyEntityManager
  , archetypes: Map.empty
  , entityLocations: Map.empty
  , componentRegistry: { labelToBit: Map.empty, nextBit: 0 }
  , structuralVersion: 0
  , queryCache: Map.empty
  }
```

to:

```purescript
emptyWorld =
  { entities: emptyEntityManager
  , archetypes: Map.empty
  , entityLocations: Map.empty
  , componentRegistry: { labelToBit: Map.empty, nextBit: 0 }
  , structuralVersion: 0
  , queryCache: Map.empty
  , maskCache: Map.empty
  }
```

(3f) Add the `resolveQueryMasks` function. Insert it after `makeQueryCacheKey` (after line 545):

```purescript
-- | Resolve a query's required/excluded label sets to bitmasks, memoised on
-- | `world.maskCache`. A cache hit avoids re-folding both label sets against
-- | the registry on every query call (the P1 cost). The entry is valid as
-- | long as the registry's `nextBit` is unchanged — a label's bit never
-- | changes once assigned, and only registry growth can flip a strict
-- | resolution from `Nothing` to `Just`.
-- |
-- | Returns the (possibly cache-updated) world so callers that thread the
-- | world (`runQueryCached`) keep the populated cache; callers that don't
-- | (`runQuery`) may discard `world`.
resolveQueryMasks
  :: Set String -> Set String -> World
  -> { required :: Maybe ComponentMask, excluded :: ComponentMask, world :: World }
resolveQueryMasks requiredLabels excludedLabels world =
  let
    key = Tuple requiredLabels excludedLabels
    currentNextBit = world.componentRegistry.nextBit
  in
    case Map.lookup key world.maskCache of
      Just cached | cached.nextBit == currentNextBit ->
        { required: cached.required, excluded: cached.excluded, world }
      _ ->
        let
          required = strictMask requiredLabels
          excluded = lenientMask excludedLabels
          entry = { required, excluded, nextBit: currentNextBit }
          newCache = Map.insert key entry world.maskCache
        in
          { required, excluded, world: world { maskCache = newCache } }
  where
    -- Strict: returns Nothing if any label is unregistered (no matches possible).
    strictMask labels =
      foldl
        (\maybeAcc label -> case maybeAcc of
            Nothing -> Nothing
            Just acc -> case Map.lookup label world.componentRegistry.labelToBit of
              Just bit -> Just (maskAddBit acc bit)
              Nothing -> Nothing)
        (Just emptyMask)
        labels

    -- Lenient: unregistered labels are ignored (missing exclusion = no exclusion).
    lenientMask labels =
      foldl
        (\mask label -> case Map.lookup label world.componentRegistry.labelToBit of
            Just bit -> maskAddBit mask bit
            Nothing -> mask)
        emptyMask
        labels
```

- [ ] **Step 4: Route `Query.purs` through `resolveQueryMasks` and delete the local folds**

In `/home/toby/pure-ecs/src/ECS/Query.purs`:

(4a) Update the `ECS.World` import (line 37) to bring in `resolveQueryMasks` and drop the now-unused `emptyMask`/`maskAddBit`:

```purescript
import ECS.World (World, Entity, ArchetypeId, Archetype, ComponentMask, QueryCacheKey, CachedQueryResult, wrapEntity, maskContains, maskHasAny, makeQueryCacheKey, resolveQueryMasks)
```

(4b) Remove the now-unused `Data.Foldable (foldl) as F` import (line 31). Delete that line entirely. (Keep `import Data.Array (foldl)` on line 29 — `mapQuery` still uses it.)

(4c) Replace `runQuery` (lines 126-147) with:

```purescript
runQuery (Query q) world =
  let
    resolved = resolveQueryMasks q.requiredLabels q.excludedLabels world
  in
    case resolved.required of
      Nothing -> []  -- Required component not registered, no matches possible
      Just requiredMask ->
        let
          excludedMask = resolved.excluded
          -- Filter archetypes whose mask matches required/excluded constraints,
          -- threading the (archId, arch) pair to avoid a reverse lookup.
          matches :: Array (ArchetypeId /\ Archetype)
          matches = Array.filter
            (\(_ /\ arch) -> archetypeMatchesMask arch.mask requiredMask excludedMask)
            (Map.toUnfoldable world.archetypes)
        in
          Array.concatMap
            (\(_ /\ arch) -> extractEntities (Proxy :: Proxy rl) arch)
            matches
```

(`runQuery` is the uncached variant and returns only an `Array`, so it discards `resolved.world` — the populated mask cache isn't retained, which is acceptable for the non-hot path.)

(4d) Delete the now-superseded `labelsToMaskStrict` and `labelsToMask` functions (lines 149-170 inclusive, plus their two doc-comment blocks at 149-150 and 162-163).

(4e) Replace `runQueryCached` (lines 199-244, keep the `where checkCache` block) with:

```purescript
runQueryCached (Query q) world =
  let
    resolved = resolveQueryMasks q.requiredLabels q.excludedLabels world
    world1   = resolved.world  -- world with the mask cache possibly updated
  in
    case resolved.required of
      Nothing -> { results: [], world: world1 }  -- Required component not registered
      Just requiredMask ->
        let
          excludedMask = resolved.excluded
          cacheKey = makeQueryCacheKey requiredMask excludedMask
          cacheResult = checkCache cacheKey world1

          { matchingArchIds, world': worldAfterCache } = case cacheResult of
            Just archIds ->
              { matchingArchIds: archIds, world': world1 }
            Nothing ->
              let
                archIds = Array.mapMaybe
                  (\(archId /\ arch) ->
                      if archetypeMatchesMask arch.mask requiredMask excludedMask
                        then Just archId
                        else Nothing)
                  (Map.toUnfoldable world1.archetypes)
                newCacheEntry = { matchingArchetypes: archIds, version: world1.structuralVersion }
                updatedCache = Map.insert cacheKey newCacheEntry world1.queryCache
              in
                { matchingArchIds: archIds
                , world': world1 { queryCache = updatedCache }
                }

          results = Array.concatMap
            (\archId -> case Map.lookup archId worldAfterCache.archetypes of
                Nothing   -> []
                Just arch -> extractEntities (Proxy :: Proxy rl) arch)
            matchingArchIds
        in
          { results, world: worldAfterCache }
  where
    checkCache :: QueryCacheKey -> World -> Maybe (Array ArchetypeId)
    checkCache key w =
      case Map.lookup key w.queryCache of
        Nothing -> Nothing
        Just cached ->
          if cached.version == w.structuralVersion
            then Just cached.matchingArchetypes
            else Nothing  -- Stale cache entry
```

(Note: everything after resolution reads `world1`, not `world`, so the populated mask cache is never lost.)

- [ ] **Step 5: Build and test**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -25
```

Expected: all tests pass (including the Win-A guard test and S5). No "unused import" surprises — if the compiler warns about `emptyMask`/`maskAddBit`/`F` still imported in `Query.purs`, remove the offending import (Step 4a/4b should have handled them).

- [ ] **Step 6: Bench and commit**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-winA-part1.txt
git add src/ECS/World.purs src/ECS/Query.purs test/QuerySpec.purs bench-after-winA-part1.txt
git commit -m "perf: memoise query mask resolution on the World (Win A part 1, P1)

runQueryCached re-folded both label sets against the component registry
on every call, before the cache lookup, so even a cache hit paid a
Map.lookup per label. Add a World-level maskCache keyed by the
(required, excluded) label sets and guarded by registry nextBit; a
label's bit never changes once assigned, so the resolution is stable
until the registry grows. runQuery/runQueryCached now share this via
World.resolveQueryMasks. (The audit's alternative -- caching masks on
the Query value -- is a no-op, since Query is rebuilt per call.)
See bench-after-winA-part1.txt.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 4: Win A part 2 — single `reflectSymbol` per component via `unsafeSet`

`readComponentsCons` (`Query.purs:337-343`) reflects each component's label twice per entity: once explicitly at `:339` for `readColumnAt`, and again inside `Record.insert` (which is `unsafeSet (reflectSymbol l) …` — confirmed in `record/Record.purs:103`, and `reflectSymbol` is *not* inlined to a literal by `purs-backend-es`). Reuse the already-bound `labelStr`. The `Cons`/`Lacks` constraints already in scope discharge exactly what `Record.insert` relies on, so calling `unsafeSet` directly with the same string is safe and produces a byte-identical record.

This is behaviour-identical, so existing query tests are the correctness guard.

**Files:**
- Modify: `src/ECS/Query.purs` (import; `readComponentsCons` body)

- [ ] **Step 1: Swap the import**

In `/home/toby/pure-ecs/src/ECS/Query.purs`, replace the `Record` import (line 42):

```purescript
import Record as Record
```

with:

```purescript
import Record.Unsafe (unsafeSet)
```

(`Record.insert` was the only use of `Record` in this module — it's about to go.)

- [ ] **Step 2: Use the already-bound label in the insert**

Replace the `readComponentsCons` instance body (lines 337-343) with:

```purescript
  readComponents _ arch rowIdx =
    let
      labelStr       = reflectSymbol (Proxy :: Proxy label)
      componentValue = readColumnAt labelStr arch rowIdx
      rest           = readComponents (Proxy :: Proxy tail) arch rowIdx
    in
      -- Reuse labelStr instead of Record.insert, which would reflect the
      -- symbol a second time. The Cons/Lacks constraints on this instance
      -- prove `label` is absent from `rest` and that r = r' + label, which is
      -- exactly the safety obligation Record.insert discharges internally.
      unsafeSet labelStr componentValue rest
```

- [ ] **Step 3: Build and test**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -20
```

Expected: all tests pass. Query result contents are unchanged (the existing `QuerySpec` field-access assertions cover this).

- [ ] **Step 4: Bench and commit**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-after-winA-part2.txt
git add src/ECS/Query.purs bench-after-winA-part2.txt
git commit -m "perf: reflect each component label once in readComponents (Win A part 2, P2)

Record.insert re-reflects the label Proxy to a string on every insert
(record/Record.purs:103), and purs-backend-es does not inline it to a
literal -- so readComponentsCons reflected each component twice per
entity. Reuse the labelStr already bound for readColumnAt via
Record.Unsafe.unsafeSet; the Cons/Lacks constraints make this identical
to Record.insert. (The per-component record clone itself remains -- that
is Win B's target, gated in Task 6/8.) See bench-after-winA-part2.txt.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 5: Close S1 — make `readColumnAt` fail loud instead of fabricating a value

`readColumnAt` (`Query.purs:349-355`) returns `componentFromForeign (componentToForeign unit)` (= `unsafeCoerce unit`, typed as the component `a`) when a column is missing or the row index is out of range. Both arms are unreachable for a well-formed query (the archetype already matched by mask ⇒ the required column exists; `mapWithIndex` keeps the row index in range), but they rest on runtime invariants the type system doesn't enforce. Today an invariant break is **silent wrong data**; the review's S4 recommendation is to make internal-invariant arms **loud**. These arms are internal-invariant (not reachable via valid public input), so crash with a diagnostic. This is decoupled from Win B (the audit folded them together) and is dead code on the happy path.

**Files:**
- Modify: `src/ECS/Query.purs` (import; `readColumnAt` body)
- Possibly: `spago.yaml` (add `partial` dependency if not transitively present)

- [ ] **Step 1: Add the `unsafeCrashWith` import**

In `/home/toby/pure-ecs/src/ECS/Query.purs`, add to the imports (after the `Prelude` import, near the top):

```purescript
import Partial.Unsafe (unsafeCrashWith)
```

- [ ] **Step 2: Replace the fabrication arms**

Replace `readColumnAt` (lines 349-355) with:

```purescript
readColumnAt :: forall a. String -> Archetype -> Int -> a
readColumnAt label arch rowIdx =
  case CS.lookup label arch.storage of
    Nothing  ->
      unsafeCrashWith $
        "ECS.Query.readColumnAt: required column '" <> label
          <> "' missing from a matched archetype "
          <> "(mask/labels/storage invariant violated)"
    Just col -> case CS.arrayIndex rowIdx col of
      Nothing ->
        unsafeCrashWith $
          "ECS.Query.readColumnAt: row index " <> show rowIdx
            <> " out of range for column '" <> label
            <> "' (entity-array/row-index invariant violated)"
      Just fv -> CS.componentFromForeign fv
```

- [ ] **Step 3: Build; add `partial` if the import is missing**

```bash
cd /home/toby/pure-ecs
spago build 2>&1 | tail -20
```

If the build reports `Module Partial.Unsafe was not found` (or a missing-dependency error for `partial`), add `partial` to the package dependency list in `/home/toby/pure-ecs/spago.yaml` (alphabetically, between `ordered-collections` and `prelude`):

```yaml
    - ordered-collections
    - partial
    - prelude
```

then re-run `spago build`. If the build already succeeded, skip this — `partial` is present transitively.

- [ ] **Step 4: Test**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -20
```

Expected: all tests pass. The crash arms are dead code on every happy path, so the suite is unaffected; constructing a triggering case would require bypassing the public API (which maintains the invariants), so no new test is added — green tests prove the happy path doesn't regress.

- [ ] **Step 5: Commit**

```bash
cd /home/toby/pure-ecs
git add src/ECS/Query.purs spago.yaml spago.lock
git commit -m "fix: readColumnAt fails loud on invariant break instead of fabricating a value (S1)

The missing-column / out-of-range arms returned unsafeCoerce unit typed
as the component, turning an internal-invariant break into silent wrong
data. Both are unreachable for a well-formed query, so replace them with
unsafeCrashWith carrying a diagnostic: dead code on the happy path, a
loud located failure if an invariant ever drifts. Decoupled from Win B
(which later makes even this unreachable by construction).

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

(If Step 3 did not modify `spago.yaml`/`spago.lock`, drop them from the `git add`.)

---

## Task 6: Build the gate — a read-heavy bench scenario for Win B

The review's central process correction: **measure before committing 3-5 days to Win B.** Win B removes the per-entity record allocation in result extraction. To decide if that's worth it *after* Win A, add a scenario that materializes a wide record (3 fields) but uses only 2 — the unused field's materialization is exactly Win B's target. Compare it to the existing 2-field `fullTick1k`.

**Files:**
- Modify: `bench/src/ECS/Bench.purs` (add `wideMovementSystem` + `readHeavyTick1k`, export it)
- Modify: `bench/src/Main.purs` (import + add to `scenarios`)

- [ ] **Step 1: Add the wide-query scenario**

In `/home/toby/pure-ecs/bench/src/ECS/Bench.purs`:

(1a) Add `readHeavyTick1k` to the export list (line 5 area):

```purescript
module Bench.ECS.Bench
  ( Scenario
  , spawn1k
  , queryAll1k
  , fullTick1k
  , readHeavyTick1k
  ) where
```

(1b) Add the wide system and scenario at the end of the file (after `fullTick1k`, line 72):

```purescript
-- | Queries a 3-field record (position, velocity, health) but writes only
-- | position from velocity -- the health field is materialized into every
-- | result record and never read. The wall-clock delta vs fullTick1k (which
-- | queries a 2-field record) isolates the per-entity record-materialization
-- | cost that Win B targets.
wideMovementSystem :: System ( position :: Position, velocity :: Velocity, health :: Health )
                              ( position :: Position )
                              Unit
wideMovementSystem = do
  rs <- queryFor @( position :: Position, velocity :: Velocity, health :: Health )
  for_ rs \r ->
    modifyComponent_ (Proxy :: _ "position")
      (\p -> { x: p.x + r.components.velocity.x
             , y: p.y + r.components.velocity.y })
      r.entity

readHeavyTick1k :: Scenario
readHeavyTick1k =
  { name: "10 wide-query ticks (pos,vel,health; uses 2) over 1000 entities"
  , build: buildWorld 1000
  , run: \w -> tickN 10 w
  }
  where
    tickN 0 w = w
    tickN k w = tickN (k - 1) (runSystem wideMovementSystem w).world
```

- [ ] **Step 2: Register it in the bench main**

In `/home/toby/pure-ecs/bench/src/Main.purs`:

(2a) Change the import (line 5):

```purescript
import Bench.ECS.Bench (Scenario, spawn1k, queryAll1k, fullTick1k, readHeavyTick1k)
```

(2b) Change the `scenarios` list (line 17):

```purescript
scenarios :: Array Scenario
scenarios = [ spawn1k, queryAll1k, fullTick1k, readHeavyTick1k ]
```

- [ ] **Step 3: Run the gate bench**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-gate-winB.txt
```

Expected: four lines now, including both `10 movement ticks` (2-field) and `10 wide-query ticks` (3-field).

- [ ] **Step 4: Record the gate decision**

Compare the two tick lines in `bench-gate-winB.txt`. **Decision rule:**
- If `readHeavyTick1k` is **>~20% slower** than `fullTick1k` (the extra field's materialization is a material fraction of the loop) → Win B (Task 8) is justified; proceed.
- If the delta is **within noise (<~10%)** → after Win A, record materialization is not where the time goes; **do not** implement Win B — the new API surface would be pure debt. Mark Task 8 "not triggered."

Write the observed numbers and the decision into the commit body.

- [ ] **Step 5: Commit**

```bash
cd /home/toby/pure-ecs
git add bench/src/ECS/Bench.purs bench/src/Main.purs bench-gate-winB.txt
git commit -m "bench: add wide-query scenario to gate Win B on record-alloc cost

Reinstates the gate the 2026-04-29 plan attached to streaming queries
(== the audit's Win B) and the audit dropped. readHeavyTick1k queries a
3-field record but uses 2; the delta vs fullTick1k isolates the
per-entity record materialization Win B targets. Decision recorded:
<paste the two tick numbers + go/no-go here>.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Task 7 (GATED): P3 — per-signature query-cache invalidation

**Trigger:** Implement this only if your workload spawns entities with *novel component combinations* during play (new archetypes mid-session: waves, projectiles, status effects). Today any new archetype bumps the single global `structuralVersion`, invalidating **every** cached query signature (`Query.purs:252` rejects all entries whose `version != structuralVersion`). P3's cost grows with session length, which a 15-second profile barely samples — so this is gated on workload knowledge, not the microbench (a faithful churn microbench needs dynamically-generated component types, which the typed harness can't express).

**Design (when triggered):** Replace the scalar `version` check with a per-archetype-set check. The cleanest low-risk option: tag each archetype with the `structuralVersion` at which it was created (or keep a monotonically-growing archetype generation), and have a cached entry remember the **set of archetype ids it matched** plus the version; on lookup, a new archetype invalidates an entry only if the new archetype *would have matched* that entry's `(requiredMask, excludedMask)`. Concretely, cache `{ matchingArchetypes, reqMask, excMask, version }` and, on a version mismatch, re-scan **only** archetypes created since `version` (not the whole map) and union them in if they match — turning "rebuild every signature from scratch" into "incrementally extend the signatures a new archetype actually affects."

**TDD target (write this test first when triggered):** two distinct query signatures A and B; populate both caches; spawn a new archetype that matches **only** B's signature; assert A's cached result is reused unchanged (observable today only via behaviour — A returns the same entities without a full re-scan; instrument `resolveQueryMasks`/cache hit counters if you need a white-box assertion).

**Effort/risk:** ~1 day, medium-low risk, no public API change. The review flagged this as **possibly a better 6.6%-side investment than Win B** and lower risk — cost it against Win B before doing either.

When triggered, expand this task into its own full plan (`docs/superpowers/plans/<date>-p3-per-signature-cache.md`) with the same bite-sized TDD structure as Tasks 2-5. Do **not** implement it inline from this stub.

---

## Task 8 (GATED): Win B — column-iteration query API

**Trigger:** Implement only if Task 6's gate fired (read-heavy wide-query loop is materially slower than the narrow one *after* Win A). If the gate did not fire, **stop** — adding a second query entry point is permanent API/doc/`ECS.Docs` surface debt for no measured win.

**Corrected design (per the PureScript reviewer — the audit's sketch was incomplete):**

```purescript
-- Resolve each required column array ONCE per archetype, then iterate rows
-- with direct column[rowIdx] reads -- no per-entity record allocation, and a
-- missing required column skips the archetype (so the read is total: this is
-- what closes S1 by construction).
forEachRow
  :: forall required excluded rl a
   . RowToList required rl
  => ExtractLabels rl
  => ReadColumns rl required          -- NOTE: needs a `| rl -> required` fundep
  => Query required excluded
  -> (EntityId -> Record required -> a -> a)   -- fold step
  -> a -> World -> { result :: a, world :: World }
```

Key corrections over the audit's sketch:
1. **Add the `RowToList`/`ExtractLabels` constraints and a `| rl -> required` functional dependency** on the new `ReadColumns` class (mirror the existing `class ReadComponents (rl :: RowList Type) (r :: Row Type) | rl -> r`), or `required` is undetermined and you get ambiguous-type errors.
2. **Resolve columns once per archetype and short-circuit** any archetype missing a required column (`ResolveColumns rl => Archetype -> Maybe (Array (Tuple String ComponentArray))`). This — *skipping* a missing column rather than carrying a sentinel into the row loop — is what makes the per-row read total and discharges S1 structurally. A skipped archetype yields no rows, which is the *sound* denotation; the old code fabricated `unit`, which was not.
3. **The `Record required` in the callback is still materialized per row.** If eliminating the record allocation (not just the repeated per-(entity,component) `Map` lookups) is the actual goal, the optimizer will **not** drop an unused record build — expose the resolved columns to the callback instead of a built `Record required`, or document plainly that "a fold that ignores fields it doesn't use still builds the record."
4. Keep `runQuery`/`runQueryCached` for back-compat; `forEachRow` is additive, opt-in.

**Effort/risk:** 3-5 days, medium risk (the failure modes are inference ambiguity from the fundep and getting the skip-on-missing-column totality argument airtight — both surface at compile time). Note S1 is **already** closed cheaply by Task 5; Win B's S1 benefit is then "unreachable by construction" rather than "loud crash," a refinement, not the primary justification.

When triggered, expand this into its own full plan (`docs/superpowers/plans/<date>-winB-column-iteration.md`) with bite-sized TDD steps. Do **not** implement it inline from this stub.

---

## Task 9: Record the audit corrections + update docs

Keep the historical record honest and the docs accurate.

**Files:**
- Modify: `PROFILER_AUDIT_2026-06-16.md` (append an addendum)
- Modify: `CLAUDE.md` and `src/ECS/CLAUDE.md` (note the mask cache + S5/S1)

- [ ] **Step 1: Append a corrections addendum to the audit**

Add to the end of `/home/toby/pure-ecs/PROFILER_AUDIT_2026-06-16.md`:

```markdown

---

## Addendum — review corrections (2026-06-16)

A five-agent review of this audit (PureScript mechanics, architecture, FP
safety, citation fact-check, repo state) confirmed the findings are accurate
(16/16 cited locations land, 0 wrong) and made three corrections, now
implemented in `docs/superpowers/plans/2026-06-16-profiler-audit-followup.md`:

1. **Win A option "cache masks on the `Query` value" is a no-op** — `Query`
   is rebuilt fresh on every `queryFor @row` call, so nothing cached on it
   survives. Implemented only the `World`-level mask-resolution cache.
2. **The P1/P2/P3 percentages are ordinal, not additive** — they nearly sum
   to 100% of the 6.6% slice and come from one unreproduced profile. Treat as
   "P1 and P2 dominate"; every perf claim is now backed by a bench delta.
3. **Win B is the 2026-04-29 plan's deferred streaming-query item, and stays
   gated** — implemented only if a post-Win-A read-heavy bench shows record
   materialization is still material.

The review also found **S5** (not in this audit): `spawnEntityPure` omitted
the `structuralVersion` bump on first empty-archetype creation, staling a
query cached before the first spawn. Fixed. And **S1**'s fix was decoupled
from Win B and closed cheaply via `unsafeCrashWith`.
```

- [ ] **Step 2: Note the mask cache and fixes in both CLAUDE.md files**

In both `/home/toby/pure-ecs/CLAUDE.md` and `/home/toby/pure-ecs/src/ECS/CLAUDE.md`, in the Phase 4 Query section's key-functions/architecture notes, add a bullet:

```markdown
- **Mask-resolution cache:** `runQueryCached` memoises each query's
  label-set → bitmask resolution on `world.maskCache`, guarded by the
  registry's `nextBit`, so a cache hit no longer re-folds the label set per
  call (`ECS.World.resolveQueryMasks`).
```

And in the "Runtime checks" / safety notes, add:

```markdown
- `readColumnAt` crashes loud (`unsafeCrashWith`) on a missing column or
  out-of-range row — an internal-invariant break surfaces as a located
  failure, not silent wrong data.
```

- [ ] **Step 3: Commit**

```bash
cd /home/toby/pure-ecs
git add PROFILER_AUDIT_2026-06-16.md CLAUDE.md src/ECS/CLAUDE.md
git commit -m "docs: record audit corrections + mask cache / S5 / S1 in guides

Addendum to the 2026-06-16 audit (review corrections + the S5 finding the
audit missed), and CLAUDE.md notes for the new World-level mask-resolution
cache and the loud readColumnAt invariant check.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
```

---

## Final verification

- [ ] **Step 1: Full suite green**

```bash
cd /home/toby/pure-ecs
npm test 2>&1 | tail -20
```

Expected: all tests pass (147+: original suite + S5 test + Win-A guard test).

- [ ] **Step 2: Bench delta vs baseline**

```bash
cd /home/toby/pure-ecs
npm run bench 2>&1 | tee bench-final.txt
diff bench-pre-followup.txt bench-final.txt || true
```

Expected: `query+update` and tick scenarios at least as fast as baseline (Win A targets cache-hit overhead, which the bench's repeated same-signature queries exercise). Document the deltas.

- [ ] **Step 3: Confirm the gate decision is recorded**

Verify Task 6's commit body (or `bench-gate-winB.txt`) states whether Win B (Task 8) is triggered, so the next session has an unambiguous go/no-go.

---

## Out of scope (deliberate)

- **Win B and P3 implementations** — gated (Tasks 7-8); expand into their own plans only when triggered.
- **Mutable typed column buffers (`STArray`/`Float64Array`)** — the 100k-entity path; a separate research-level plan, as the 2026-04-29 plan already noted.
- **Rewriting the audit's body** — it's a historical record; corrections go in the addendum, not by editing the findings.
```
