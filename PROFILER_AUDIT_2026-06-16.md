# pure-ecs Profiler Audit — 2026-06-16

**Revision audited:** `3c6fbff2f429edf4fdd614a214a5f0c7abacbb33` (repo HEAD at time of audit).
**Method:** A downstream consumer (an ECS-heavy browser autobattler running the full
pipeline at 30 ticks/s) was profiled in Firefox Profiler for ~15 s of live play. The
sampled JS was correlated back to `output-es/ECS.*` symbols and then to source.
All file:line references below were re-checked against this repo's HEAD.

> **Scope caveat.** These numbers come from **one** consumer's workload. They're a strong
> signal about where pure-ecs spends time under a read-heavy, high-tick-rate game, but the
> exact percentages are workload-specific. In that profile, pure-ecs was the **#2** cost
> (~6.6% of wall-clock); the #1 cost (~22%) was the consumer's own pathfinding, not the
> library. Don't over-index on 6.6% as a universal figure — treat the *mechanisms* as the
> durable findings.

---

## Executive summary

pure-ecs is **fundamentally sound**. The storage layer is already an archetype
struct-of-arrays, query matching is already O(1) bitmask ops, entity ids are already
generation-indexed, and the `State World` monad inlines cleanly under `purs-backend-es`.
**Do not redesign the storage, and do not remove the query cache** (it is correct — see S2).

The ~6.6% the library costs under a read-heavy 30 tps workload comes from two places, both
fixable **without changing the public API**:

1. **`runQueryCached` rebuilds the query's bitmask on every call, *before* it consults the
   cache** — so a cache *hit* still pays the full label→mask `Map`-fold. (P1)
2. **Result extraction materializes a full N-field record per matching entity per tick**,
   which read-heavy systems immediately destructure and discard. (P2)

One low-severity **safety** issue is worth closing: `readColumnAt` fabricates a value typed
as a real component (`unsafeCoerce unit`) on a missing column/index, so the type `a` can be
inhabited by garbage if the archetype/mask invariants ever drift (S1).

The single highest-leverage change — a **column-iteration query API** — fixes the largest
performance cost (P2) *and* closes the safety hole (S1) in one move: "entity in result ⇒
component column exists" becomes true by construction.

---

## What's already good (leave it alone)

- **Storage is archetype SoA.** `ComponentStorage = Object Foreign` columns
  (`src/ECS/Internal/ComponentStorage.purs:41`); each component is a dense parallel
  `Array Foreign` indexed by a shared row position. This is the layout you'd redesign
  *toward*; it's already here.
- **Query matching is O(1) bitmask.** `archetypeMatchesMask`
  (`src/ECS/Query.purs:177–180`) does `maskContains` / `maskHasAny` on precomputed
  archetype masks. `PERFORMANCE_PLAN.md` §1.1 ("Bitmask-Based Archetype Matching")
  proposes this as future work — **it has already shipped** (multi-word bitset,
  commit `2157d8a`). That section of the plan is now historical.
- **`componentTo/FromForeign` are zero-cost** (`unsafeCoerce`,
  `ComponentStorage.purs:134–141`) — no boxing on the value path.
- **Component-value writes don't invalidate the cache.** `setComponentPure` copies only
  the one touched column and does **not** bump `structuralVersion` — so steady-state value
  churn doesn't thrash the cache. Good design point.
- **Entity ids are generation-indexed** (`src/ECS/Entity.purs`), so the stale-handle /
  use-after-free class is already closed.
- **The monad is not a cost.** `System reads writes a = State World a`
  (`src/ECS/System.purs`) lowers to plain `\world -> Tuple a world` closures with no
  transformer stack.

---

## Performance findings

### P1 — `runQueryCached` rebuilds the bitmask before checking the cache
**Location:** `src/ECS/Query.purs:199–213` (mask build at `:202–203`, cache check at `:213`/`:247–254`), via `labelsToMaskStrict`/`labelsToMask` `:151–170`.
**Est. share:** ~35–45% of the 6.6%.

Every call computes:

```purescript
requiredResult = labelsToMaskStrict q.requiredLabels world   -- :202
excludedMask   = labelsToMask       q.excludedLabels world    -- :203
...
cacheKey   = makeQueryCacheKey requiredMask excludedMask      -- :210
cacheResult = checkCache cacheKey world                       -- :213
```

`labelsToMask*` folds over the query's `Set String` and does a `Data.Map.lookup` against
`world.componentRegistry.labelToBit` **per label**, every call. Because the cache is *keyed
by the resolved mask*, you must resolve the mask just to look the entry up — so a cache hit
saves the archetype scan but never the key construction. Under a workload with dozens of
`queryFor` calls per tick at 30 tps, that's thousands of string-keyed `Map` descents per
second whose only product is a cache key.

**Why it's the clearest "caching overhead shows up hot" symptom:** the cached path was
meant to make repeat queries cheap, but the per-call mask rebuild puts a fixed floor under
every cache hit.

### P2 — Per-entity record materialization in result extraction
**Location:** `src/ECS/Query.purs:330–343` (`readComponentsCons` instance), `:349–355`
(`readColumnAt`), `:362–372` (`extractEntities`).
**Est. share:** ~30–40% of the 6.6%.

`extractEntities` maps over an archetype's entity column and, per entity, builds a typed
record by walking the `RowList`:

```purescript
readComponents _ arch rowIdx =
  let labelStr       = reflectSymbol (Proxy :: Proxy label)        -- :339
      componentValue = readColumnAt labelStr arch rowIdx           -- :340
      rest           = readComponents (Proxy :: Proxy tail) arch rowIdx
  in  Record.insert (Proxy :: Proxy label) componentValue rest     -- :343
```

This is **already optimized** relative to an earlier design — the comments at `:318–323`
and `:357–361` note that per-(entity, component) `Map` traversals were removed, and column
reads are now one `CS.lookup` + one `Array.index`. The remaining cost is the **record
itself**:

- `Record.insert` clones the accumulator once per component → C record allocations (and up
  to O(C²) field copies) per entity, per tick.
- `reflectSymbol` runs **twice per component**: once explicitly at `:339`, and again inside
  `Record.insert` (which reflects the `Proxy` to get the key). The `labelStr` already
  computed at `:339` is not reused for the insert.

For read-heavy systems that immediately destructure the result into their own narrow slices,
this materialized record is pure allocation waste.

### P3 — One new archetype invalidates *all* cached queries
**Location:** cache miss path `src/ECS/Query.purs:222–228`; version bump in
`src/ECS/Component.purs:532,670` (`incrementStructuralVersion`); staleness check `Query.purs:252`.
**Est. share:** ~10–15% of the 6.6%.

`checkCache` compares a single global `world.structuralVersion`. Creating any new archetype
bumps it, which makes **every** cached query stale, so the next `queryFor` of each distinct
signature falls back to `Array.mapMaybe (…) (Map.toUnfoldable world.archetypes)` — a full
archetype-map flatten + scan. In a workload that spawns entities with novel component
combinations during play (waves, projectiles), this recurs throughout a session rather than
settling after warmup.

### P4 — (non-finding) monad / write-back overhead is negligible
Recorded for completeness: `WorldM`/`System` overhead did not show in the profile, and
`updateComponent_` copies only the touched column, not whole maps. No action.

---

## Safety findings

### S1 (LOW) — `readColumnAt` fabricates a value typed as a real component
**Location:** `src/ECS/Query.purs:349–355`.

```purescript
readColumnAt label arch rowIdx =
  case CS.lookup label arch.storage of
    Nothing  -> CS.componentFromForeign (CS.componentToForeign unit)   -- :352
    Just col -> case CS.arrayIndex rowIdx col of
      Nothing -> CS.componentFromForeign (CS.componentToForeign unit)  -- :354
      Just fv -> CS.componentFromForeign fv
```

The fallback arms return `unsafeCoerce unit` **typed as `a`** — i.e. the signature promises a
valid component, but a missing column or out-of-range row hands back a fabricated value with
no error. It is **unreachable for a well-formed query** (entity in a matching archetype ⇒ the
required column exists ⇒ row index in range), but that guarantee rests on three runtime
invariants the type system does not enforce (mask ⇔ labels ⇔ columns agreement, and
entity-array ⇔ row-index agreement). If any drifts, this surfaces as **silent wrong data**,
not a crash — and the library already ships a debug tool (`ECS.Debug`, "‼ DIVERGENCE") whose
job is to catch exactly that drift, which tells you it's considered possible.

This is a "make illegal states unrepresentable" gap. The fix is structural — see Win B.

### S2 (NONE) — the query cache is CORRECT
This was the prime suspect (a cache that returns stale results after entity churn). **It does
not have that bug.** The cache stores **archetype ids**, not resolved entities or component
values (`Query.purs:230` `matchingArchetypes: archIds`), and results are *always* re-extracted
from live `world.archetypes` (`:238–242`):

- **Spawn/despawn into an existing archetype:** no version bump, none needed — the live
  archetype's entity array is re-read, so results are current.
- **New-archetype creation:** `incrementStructuralVersion` bumps the version; `checkCache`
  (`:252`) treats the whole cache as stale. Correct (and covered by
  `test/QuerySpec.purs` "cached query sees a new high-bit archetype after structural change").
- **Despawn:** deliberately does not bump the version (no archetype is created); the now-shorter
  entity array is still read live. Correct.

The only smell is **coarseness** (one global version invalidates all entries) — that's the P3
*performance* issue, not a correctness one.

### S3 (LOW) — type-level read/write rows are advisory, not enforced
`System reads writes a` carries phantom `reads`/`writes` rows, but the type synonym **erases**
them — nothing prevents a system body from reading/writing outside its declared set. `queryFor`'s
`Union required extra reads` and `updateComponent_`'s `Cons label a trash writes` constraints do
prove the *call-site* component is in the declared row, but the rows themselves are honest only by
annotation. They're valuable as **documentation** (consumed by `ECS.Docs`) and as a future hook
for parallel-conflict analysis — just don't market them as an airtight capability system today.

### S4 (INFO) — totality
The only unsafe ops are confined to `Internal/ComponentStorage.purs` behind the documented
"every value is `Array Foreign`" invariant. No `fromJust`/`Array.head`/`unsafePartial` in hot
code. The "shouldn't happen" arms (`arraySwapRemoveAt`, `removeFromArchetype`, …) are total but
**swallow** the impossible case (return the input unchanged) rather than signalling it — which is
why an invariant break manifests as silent corruption (cf. S1) instead of a loud failure.

---

## Recommendations

### Win A — cheap fork patch (≈1 day, low risk, no API change)
Targets P1 + the double-reflect half of P2. Reclaims an estimated ~1.5–2 of the 6.6%.

1. **Stop rebuilding the mask on every cached call.** Memoize the `(requiredMask, excludedMask)`
   resolution so a cache hit doesn't re-fold the label set. Options, cheapest first:
   - cache the resolved masks on the `Query` value, keyed by the registry's `nextBit` (the mask
     is only invalid if the registry grew); or
   - keep a `Map (Set String) (reqMask, excMask)` resolution cache on the `World` alongside
     `queryCache`.
2. **Compute each label once in `readComponents`.** Reuse the `labelStr` already bound at
   `Query.purs:339` for the insert via `Record.Unsafe.unsafeSet labelStr` (the `Cons`/`Lacks`
   constraints already discharge its safety) instead of `Record.insert (Proxy)` re-reflecting the
   symbol.

### Win B — column-iteration query API (≈3–5 days, medium risk) — the high-leverage one
Targets the rest of P2 (the dominant allocation) **and** closes S1.

Add a streaming/folding query that resolves each archetype's columns **once** and then iterates
row indices, instead of building a `QueryResult` record per entity. Sketch:

```purescript
-- Resolve each required column array ONCE per archetype, then iterate rows
-- with direct column[rowIdx] reads — no per-entity record allocation.
forEachRow
  :: forall required rl a
   . RowToList required rl => ReadColumns rl required
  => Query required excluded
  -> (EntityId -> Record required -> a -> a)   -- fold; record materialized only if the consumer keeps it
  -> a -> World -> a
```

Why it's "fast *and* safe by design":
- **Faster:** turns P2's O(entities × components) record build into O(archetypes × components)
  column resolution + O(entities) raw index reads. Read-heavy systems that touch a few fields stop
  paying for the unused ones.
- **Safer:** resolving columns once per archetype means the "column exists" check happens **once,
  up front** — a missing required column fails/skips the archetype structurally, so the per-row read
  is **total** and the `unsafeCoerce unit` fallback (S1) disappears.

Keep `runQueryCached`/`runQuery` for back-compat; migrate systems to `forEachRow` opt-in. This
relates to `tickets/01-type-directed-query-api.md` (query API surface) but is a distinct,
additive entry point.

### Verdict
**Fork-and-patch, not redesign.** Land **Win A** now (cheap, low risk, no API change), and schedule
**Win B** as the real fix for the per-entity allocation. Leave the storage (already SoA) and the
query cache (already correct) alone. The biggest win belongs in the library, not in consumer usage —
the wasted record allocation is intrinsic to `extractEntities`/`readComponents` and no amount of
consumer-side discipline avoids it while using `runQuery*`.

---

## Quick reference — verified locations (HEAD `3c6fbff`)

| ID | Issue | File:line |
|----|-------|-----------|
| P1 | mask rebuilt before cache check | `src/ECS/Query.purs:199–213`, `:151–170` |
| P2 | per-entity record materialization | `src/ECS/Query.purs:330–343`, `:349–355`, `:362–372` |
| P3 | global version invalidates all cached queries | `src/ECS/Query.purs:222–228,252`; `src/ECS/Component.purs:532,670` |
| S1 | `readColumnAt` fabricates `a` on miss | `src/ECS/Query.purs:349–355` |
| S2 | query cache (verified correct) | `src/ECS/Query.purs:230,238–242,247–254` |
| S3 | advisory read/write rows | `src/ECS/System.purs` |
| — | SoA storage (good) | `src/ECS/Internal/ComponentStorage.purs:41` |

*Audit performed against repo HEAD `3c6fbff`; cross-referenced with `PERFORMANCE_PLAN.md`
(its §1.1 bitmask work is already landed) and `tickets/01`.*

---

## Addendum — review + implementation outcome (2026-06-16)

A five-agent review (PureScript mechanics, architecture, FP safety, citation
fact-check, repo state) confirmed this audit is accurate — 16/16 cited
locations land, 0 wrong — and made corrections that are now implemented on
branch `perf/profiler-audit-followup` per
`docs/superpowers/plans/2026-06-16-profiler-audit-followup.md`:

1. **Win A option "cache masks on the `Query` value" is a no-op** — `Query` is
   rebuilt fresh on every `queryFor @row` call, so nothing cached on it
   survives. Implemented only the `World`-level mask-resolution cache
   (`ECS.World.resolveQueryMasks`, guarded by registry `nextBit`).
   *Honest non-result:* it shows **no measurable delta** on the repo
   benchmarks — their queries use 2 labels (the fold is already trivial) and
   machine noise dominates. Kept as a correct, no-regression change targeting
   the audit's many-queries-per-tick consumer, **not** a substantiated speedup.

2. **The P1/P2/P3 percentages are ordinal, not additive** — they nearly sum to
   100% of the 6.6% slice and came from one unreproduced profile. Read as
   "P1 and P2 dominate," not as bankable numbers.

3. **Win B is the 2026-04-29 plan's deferred streaming-query item, and was
   gated, not scheduled.** A read-heavy wide-query bench (added as the gate)
   measured the per-entity record materialization Win B targets at **~5%** of a
   tick — below the ~20% bar — so **Win B was NOT built**. A second query API
   would have been pure debt. (See `bench-gate-winB.txt`.)

Additional outcomes:
- **S5 (not in this audit):** `spawnEntityPure` omitted the `structuralVersion`
  bump on first empty-archetype creation, staling a query cached before the
  first spawn. **Fixed** (with a regression test).
- **S1 fix decoupled from Win B** and closed cheaply: `readColumnAt` now
  `unsafeCrashWith`s on its (unreachable) invariant-break arms instead of
  fabricating `unsafeCoerce unit`, turning silent corruption into a loud,
  located failure.
- **P3 (per-signature cache invalidation)** remains gated on workload knowledge
  (spawn-heavy, novel-archetype sessions); to be expanded into its own plan
  when triggered.
