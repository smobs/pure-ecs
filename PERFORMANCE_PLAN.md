# Pure-ECS Performance Optimization Plan

## Current Bottlenecks (Prioritized)

1. **Entity Index Lookup** - O(N) per entity in archetype
2. **Archetype ID Parsing** - String split on every query match
3. **Set Operations for Query Matching** - O(R + E) per archetype
4. **Map Lookups** - O(log N) instead of O(1)
5. **Foreign Type Erasure** - Dynamic dispatch overhead

---

## Phase 1: Quick Wins (No API Changes)

### 1.1 Bitmask-Based Archetype Matching

**Current:**
```purescript
archetypeMatches archId required excluded =
  let archLabels = parseArchetypeId archId  -- Expensive string split!
      hasRequired = required `Set.subset` archLabels
      noExcluded = Set.isEmpty (excluded `Set.intersection` archLabels)
  in hasRequired && noExcluded
```

**Proposed:**
```purescript
-- Assign each component label a unique bit
type ComponentMask = Int  -- 32 components max, or use BigInt for more

-- Component registry (built once at startup)
type ComponentRegistry = { labelToMask :: Map String Int, nextBit :: Int }

-- Archetype stores precomputed mask
type Archetype =
  { entities :: Array EntityId
  , storage :: ComponentStorage
  , mask :: ComponentMask  -- NEW: Precomputed bitmask
  }

-- Query matching becomes O(1) bitwise ops
archetypeMatches :: ComponentMask -> ComponentMask -> ComponentMask -> Boolean
archetypeMatches archMask requiredMask excludedMask =
  (archMask .&. requiredMask == requiredMask) &&
  (archMask .&. excludedMask == 0)
```

**Impact:** Query matching from O(A × C) → O(A) where C = components per archetype

**Files to modify:**
- `src/ECS/Internal/ComponentStorage.purs` - Add ComponentMask type
- `src/ECS/World.purs` - Add mask to Archetype, maintain registry
- `src/ECS/Query.purs` - Use bitmask matching

---

### 1.2 Entity Position Map in Archetype

**Current:**
```purescript
-- Finding entity in archetype: O(entities)
findIndex (\e -> e == entityId) arch.entities
```

**Proposed:**
```purescript
type Archetype =
  { entities :: Array EntityId
  , entityPositions :: Map Int Int  -- NEW: entityIndex → array position
  , storage :: ComponentStorage
  , mask :: ComponentMask
  }

-- Lookup becomes O(log N) instead of O(N)
lookupEntityPosition :: EntityId -> Archetype -> Maybe Int
lookupEntityPosition (EntityId {index}) arch =
  Map.lookup index arch.entityPositions
```

**Impact:** Entity operations from O(N) → O(log N)

**Files to modify:**
- `src/ECS/World.purs` - Add entityPositions to Archetype
- `src/ECS/Component.purs` - Update on add/remove

---

### 1.3 Cache Archetype ID Parsing

**Current:** Parse `"Health,Position,Velocity"` on every access

**Proposed:**
```purescript
type Archetype =
  { entities :: Array EntityId
  , entityPositions :: Map Int Int
  , storage :: ComponentStorage
  , mask :: ComponentMask
  , labels :: Set String  -- NEW: Cached parsed labels
  }
```

**Impact:** Remove repeated string splitting

---

## Phase 2: Structural Improvements

### 2.1 Command Buffer for Batch Operations

**Problem:** Adding component = immediate archetype migration = expensive

**Solution:** Defer structural changes to end of frame

```purescript
data WorldCommand
  = SpawnEntity (Entity () -> State World Unit)
  | DespawnEntity EntityId
  | AddComponent String Foreign EntityId
  | RemoveComponent String EntityId

type CommandBuffer = Array WorldCommand

-- Accumulate commands during system execution
addComponentDeferred :: Proxy label -> a -> Entity r -> System r w Unit
addComponentDeferred proxy val entity = do
  pushCommand (AddComponent (reflectSymbol proxy) (toForeign val) (entityId entity))

-- Apply all at frame end
flushCommands :: CommandBuffer -> World -> World
flushCommands commands world = foldl applyCommand world commands
```

**Benefits:**
- Batch similar operations (all adds to same archetype)
- Avoid repeated archetype migrations
- Enable parallel query execution (no mutations during iteration)

**Files to add:**
- `src/ECS/CommandBuffer.purs` - New module

---

### 2.2 Sparse Set for Entity-to-Archetype Mapping

**Current:** `Map Int ArchetypeId` - O(log N) lookup

**Proposed:** Sparse array with direct indexing

```purescript
-- Dense-Sparse set pattern
type EntityLocations =
  { sparse :: Array (Maybe Int)  -- entityIndex → dense index
  , dense :: Array { entityIndex :: Int, archetypeId :: ArchetypeId }
  }

-- O(1) lookup
lookupArchetype :: Int -> EntityLocations -> Maybe ArchetypeId
lookupArchetype idx locs = do
  denseIdx <- Array.index locs.sparse idx >>= identity
  entry <- Array.index locs.dense denseIdx
  pure entry.archetypeId
```

**Impact:** Entity-to-archetype from O(log N) → O(1)

---

## Phase 3: Hot Path Optimization (ST Monad)

### 3.1 Mutable Query Iteration

For the innermost loop only, use ST for local mutation:

```purescript
import Control.Monad.ST as ST
import Data.Array.ST as STA

-- Hot system path with mutable accumulator
runSystemST :: forall r w a. System r w a -> World -> { world :: World, result :: a }
runSystemST system world = ST.run do
  -- Use STArray for intermediate results
  results <- STA.new
  -- ... mutable iteration ...
  frozen <- STA.freeze results
  pure { world: world', result: ... }
```

**Key insight:** ST provides local mutation that "escapes" as pure value. The API remains pure, only internals use mutation.

---

### 3.2 TypedArray Component Storage (Advanced)

For numeric components only, use JavaScript TypedArrays:

```purescript
-- Specialized storage for numeric archetypes
foreign import data Float32Array :: Type

type NumericArchetype =
  { posX :: Float32Array
  , posY :: Float32Array
  , velX :: Float32Array
  , velY :: Float32Array
  }

-- FFI for hot loop
foreign import updatePositionsFFI :: Float32Array -> Float32Array -> Float32Array -> Float32Array -> Number -> Effect Unit
```

**Trade-off:** Breaks generic component system, only for known hot paths

---

## Phase 4: Query Caching

### 4.1 Memoized Query Results

```purescript
type CachedQuery r =
  { requiredMask :: ComponentMask
  , excludedMask :: ComponentMask
  , matchingArchetypes :: Array ArchetypeId  -- Cached!
  , version :: Int  -- Invalidate on world structural change
  }

-- World tracks structural version
type World =
  { ...
  , structuralVersion :: Int  -- Incremented on archetype changes
  , queryCache :: Map QueryKey (Array ArchetypeId)
  }
```

**Impact:** Repeated queries skip archetype matching entirely

---

## Implementation Priority

| Phase | Task | Effort | Impact | Risk |
|-------|------|--------|--------|------|
| 1.1 | Bitmask matching | Medium | High | Low |
| 1.2 | Entity position map | Low | High | Low |
| 1.3 | Cache archetype labels | Low | Medium | Low |
| 2.1 | Command buffer | Medium | Medium | Medium |
| 2.2 | Sparse set locations | Medium | Medium | Low |
| 3.1 | ST hot paths | High | High | Medium |
| 3.2 | TypedArray storage | High | Very High | High |
| 4.1 | Query caching | Medium | High | Low |

---

## Recommended Order

1. **Phase 1.2** - Entity position map (quick, high impact)
2. **Phase 1.1** - Bitmask matching (enables fast queries)
3. **Phase 1.3** - Cache labels (trivial after 1.1)
4. **Phase 4.1** - Query caching (builds on 1.1)
5. **Phase 2.1** - Command buffer (API addition, non-breaking)
6. **Phase 2.2** - Sparse set (replace entityLocations)
7. **Phase 3.1** - ST hot paths (only if needed after above)
8. **Phase 3.2** - TypedArrays (last resort, breaking change)

---

## Benchmarking Strategy

Before any optimization, establish baselines:

```purescript
-- Benchmark scenarios
benchmark1KEntities :: Effect Unit  -- Spawn 1000 entities
benchmarkQueryAll :: Effect Unit    -- Query all with 2 components
benchmarkMixedOps :: Effect Unit    -- Spawn/update/despawn cycle
benchmarkArchetypeMigration :: Effect Unit  -- Add components to entities
```

Use `purescript-benchotron` or simple `Effect.now` timing.

---

## API Compatibility

All Phase 1-2 changes are **internal only** - no public API changes required.

Phase 3-4 may add new APIs but existing code continues to work:
- `runSystemST` alongside `runSystem`
- `addComponentDeferred` alongside `addComponent`

---

## Expected Results

After Phase 1-2 (minimal effort):
- Query matching: ~10x faster (O(1) vs O(C) per archetype)
- Entity operations: ~5x faster (O(log N) vs O(N))
- Overall: **500-1000 entities at 60 FPS realistic target**

After Phase 3-4 (significant effort):
- Hot loops: ~10-100x faster with TypedArrays
- Query execution: ~2x faster with caching
- Overall: **5000+ entities at 60 FPS possible**

---

## References

- [Flecs Performance Guide](https://github.com/SanderMertens/flecs/blob/master/docs/Queries.md)
- [Bevy ECS Internals](https://bevyengine.org/learn/book/ecs/)
- [Data-Oriented Design Book](https://www.dataorienteddesign.com/dodbook/)
- [PureScript ST Monad](https://pursuit.purescript.org/packages/purescript-st)
