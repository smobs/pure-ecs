# ECS Implementation Guide

Pure functional Entity Component System with row-polymorphic type safety.

## Quick Start

```purescript
import ECS.World (emptyWorld, spawnEntity)
import ECS.Component (addComponent)
import ECS.Query (query)
import ECS.System (System, runSystem, updateComponent)
import ECS.System as S
import Type.Proxy (Proxy(..))

-- Define components
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }

-- Create world and spawn entity
let world = emptyWorld
    {world: w1, entity: e} = spawnEntity world

-- Add components (types automatically tracked!)
    {world: w2, entity: e'} = addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e w1
    {world: w3, entity: e''} = addComponent (Proxy :: _ "velocity") {x: 1.0, y: 1.0} e' w2
    -- e'' now has type: Entity (position :: Position, velocity :: Velocity)

-- Create system using State monad
    moveSystem :: System (position :: Position, velocity :: Velocity) (position :: Position) Unit
    moveSystem = do
      results <- S.query $ query (Proxy :: _ (position :: Position, velocity :: Velocity))
      for_ results \r -> do
        void $ updateComponent (Proxy :: _ "position") newPos r.entity
      -- Type-safe access: r.components.position

-- Run system
    let {world: w4, result: _} = runSystem moveSystem w3
```

## Core Modules

### Phase 1: Entity (`ECS.Entity`)
- Versioned entity IDs prevent stale references
- Entity recycling with free list (LIFO)
- O(1) validation
- State monad for chaining operations

**Key functions**: `createEntity`, `deleteEntity`, `validateEntity`

### Phase 2: World (`ECS.World`)
- Archetype-based storage for cache efficiency
- Entities grouped by component signature
- Phantom row types track components at compile-time
- Type erasure with `Foreign` for heterogeneous storage

**Key functions**: `emptyWorld`, `spawnEntity`, `despawnEntity`, `hasEntity`

### Phase 3: Component (`ECS.Component`)
- Add/remove components with automatic archetype migration
- Type-safe component access with row constraints
- `Lacks` prevents duplicates, `Cons` proves existence
- Entity phantom type updates automatically

**Key functions**: `addComponent`, `removeComponent`, `getComponent`, `hasComponent`

**Important**: `addComponent` cannot update existing components (has `Lacks` constraint).
Use `removeComponent` then `addComponent`, or use System's `updateComponent` helper.

### Phase 4: Query (`ECS.Query`)
- Row-polymorphic queries preserve field names
- `query (Proxy :: _ (position :: Position, velocity :: Velocity))` → typed results
- Exclusion filters with `without`
- RowToList for generic component iteration

**Key functions**: `query`, `runQuery`, `without`, `forQuery`, `mapQuery`

**Type specification**: Use Proxy to specify component types:
```purescript
-- ✅ Use Proxy with type annotation
let q :: _ (position :: Position) ()
    q = query (Proxy :: _ (position :: Position))

-- ✅ Type annotation can be inferred from Proxy
let q = query (Proxy :: _ (position :: Position))
```

### Phase 5: System (`ECS.System`)
- **State monad** for clean composition: `type System reads writes a = State World a`
- Phantom types track read/write access at compile-time
- Enables future parallel execution analysis
- System composition via do-notation (automatic sequencing)

**Key functions**: `runSystem`, `query`, `updateComponent`

**API Pattern**: Systems are State monad computations that query entities and update components:
```purescript
-- Run system with runSystem
let {world: w', result: value} = runSystem mySystem world

-- Query within system
results <- S.query $ query (Proxy :: _ (position :: Position, velocity :: Velocity))

-- Update components
entity' <- updateComponent (Proxy :: _ "position") newPos entity

-- Compose systems with do-notation
combinedSystem = do
  physicsSystem
  damageSystem
  cleanupSystem
```

**Union constraint fix**: Use `Union required extra reads` instead of `Union required reads reads` for better type inference with closed rows.

## Design Philosophy

### 1. Pure Functional Core
- No mutable state
- All operations return new World
- Easy to test and reason about
- Time-travel debugging possible

### 2. Type Safety
**Compile-time guarantees**:
- Can't access components entity doesn't have
- Can't add duplicate components
- Can't remove non-existent components
- Query results have correct types
- System access patterns enforced

**Runtime checks**:
- Entity version validation
- Archetype existence
- Component storage bounds

### 3. Zero-Cost Abstractions
- Row types erased at runtime
- Phantom types have zero overhead
- Entity is just `EntityId` at runtime
- Systems are plain functions

### 4. Invariant-Based Safety (Internal Module Pattern)
**Key insight**: Library code controls internal invariants, so unsafe operations can be encapsulated.

- **Public API**: Completely type-safe, zero `unsafeCoerce` or `unsafeFromForeign`
- **Internal module** (`ECS.Internal.ComponentStorage`): Encapsulates all Foreign operations
- **Opaque types**: `ComponentStorage` hides `Object Foreign` implementation
- **Documented invariant**: Every value in storage is `Array Foreign`
- **Safe wrappers**: `lookup`, `insert`, `fold` maintain invariants

This is textbook smart constructors - the public API leverages compile-time types while internal code uses runtime knowledge about invariants for safe type erasure.

### 5. Row Polymorphism Patterns

**Lacks** - Prevents duplicates:
```purescript
addComponent :: Lacks label r => ...
-- Can only add if component doesn't exist
```

**Cons** - Proves existence:
```purescript
removeComponent :: Cons label a r' r => ...
-- Proves: r = r' + label
-- Can only remove if component exists
```

**Union** - Subset relationships:
```purescript
-- Proves: required ⊆ reads
queryInSystem :: Union required extra reads => ...
```

**RowToList** - Generic iteration:
```purescript
class ExtractLabels (rl :: RowList Type) where ...
-- Convert row to type-level list for recursion
```

## Common Patterns

### Building Entities
```purescript
-- Chain addComponent calls
let {world: w1, entity: e1} = spawnEntity world
    {world: w2, entity: e2} = addComponent (Proxy :: _ "position") pos e1 w1
    {world: w3, entity: e3} = addComponent (Proxy :: _ "velocity") vel e2 w2
    {world: w4, entity: e4} = addComponent (Proxy :: _ "health") hp e3 w3
-- e4 has type: Entity (position :: Position, velocity :: Velocity, health :: Health)
```

### Writing Systems
```purescript
-- State monad approach (current API)
movementSystem :: Number -> System (position :: Position, velocity :: Velocity)
                                     (position :: Position)
                                     Int
movementSystem dt = do
  results <- S.query $ query (Proxy :: _ (position :: Position, velocity :: Velocity))
  for_ results \r -> do
    let newPos = {x: r.components.position.x + r.components.velocity.x * dt
                 ,y: r.components.position.y + r.components.velocity.y * dt}
    void $ updateComponent (Proxy :: _ "position") newPos r.entity
  pure $ length results

-- Manual state pattern (for inline systems)
import Control.Monad.State as CMS
import Data.Tuple (Tuple(..))

simpleSystem :: System (position :: Position) () Unit
simpleSystem = CMS.state \world ->
  let results = runQuery (query (Proxy :: _ (position :: Position))) world
      -- ... manual world manipulation ...
  in Tuple unit world'
```

### Composing Systems
```purescript
-- Do-notation automatically sequences systems
gameLoop :: System (position :: Position, velocity :: Velocity, health :: Health)
                   (position :: Position, health :: Health)
                   Unit
gameLoop = do
  physicsSystem
  damageSystem
  cleanupSystem
  -- Access patterns automatically merged
```

## Performance Characteristics

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| spawnEntity | O(1) amortized | O(1) | Free list for recycling |
| addComponent | O(log A + C) | O(C) | A=archetypes, C=components |
| removeComponent | O(log A + C) | O(1) | Archetype migration |
| query | O(A × N) | O(N) | A=matching archetypes, N=entities |
| runSystem | O(system) | O(system) | Depends on system complexity |

**Optimization tips**:
- Batch entity creation
- Query once, iterate many times
- Use archetype-aware algorithms
- Profile before optimizing

## Migration Guide

### Adding to Existing Project

1. **Copy ECS/ directory** - No game dependencies
2. **Add dependencies** to spago.yaml:
```yaml
dependencies:
  - foreign
  - foreign-object
  - ordered-collections
  - transformers
```
3. **Import modules**:
```purescript
import ECS.World (World, emptyWorld, spawnEntity)
import ECS.Component (addComponent, getComponent)
import ECS.Query (query)
import ECS.System (System, runSystem, updateComponent)
import ECS.System as S
```

### Incremental Adoption

- Start with entity management
- Add component storage
- Implement one system
- Gradually port existing code
- Keep non-ECS code separate (UI, input, etc.)

## Troubleshooting

### "No instance found for Lacks"
**Problem**: Trying to add component that already exists
**Solution**: Use `updateComponent` or remove-then-add pattern

### "Could not match type" with rows
**Problem**: Using wrong entity reference after add/remove
**Solution**: Always use the returned entity from operations

### "Unknown type" with Union constraints
**Problem**: Closed rows confusing solver
**Solution**: Add type annotations or use manual `mkSystem` pattern

### Query returns empty unexpectedly
**Problem**: Component values used for type inference
**Solution**: Provide type annotation on query

## Examples

See `src/ECS/Examples/SimpleExample.purs` for a complete working example with:
- Multiple entity types
- 3 systems (physics, damage, cleanup)
- Game loop over 5 ticks
- Entity lifecycle (spawn → update → despawn)

## Testing

**110 tests covering**:
- Entity lifecycle (27 tests)
- World operations (22 tests)
- Component management (18 tests)
- Query system (27 tests)
- System execution (16 tests)

Run: `spago test`

## Dependencies

**Core**:
- `purescript-prelude`
- `purescript-effect`
- `purescript-transformers`
- `purescript-foreign`
- `purescript-foreign-object`
- `purescript-ordered-collections`

**Testing**:
- `purescript-spec`

## Future Enhancements

- Parallel system execution (access patterns ready)
- Query caching
- Bitset archetype matching (O(1))
- Save/load world state
- System dependency graph
- Hot-reloading systems
- Visual debugger
- Performance profiling

## References

- PureScript row types: https://github.com/purescript/documentation/blob/master/language/Types.md#rows
- RowToList patterns: PureScript by Example, Ch. 13
- Archetype ECS: "Data-Oriented Design" by Richard Fabian
- Type-level programming: "Thinking with Types" concepts

## Complete Example

```purescript
module Game where

import Prelude
import ECS.World as ECS
import ECS.Component (addComponent)
import ECS.Query (query)
import ECS.System (System, runSystem, updateComponent)
import ECS.System as S
import Type.Proxy (Proxy(..))
import Data.Traversable (for_)

-- 1. Define components
type Position = {x :: Number, y :: Number}
type Velocity = {x :: Number, y :: Number}

-- 2. Setup world
setupWorld = do
  let w0 = ECS.emptyWorld
      {world: w1, entity: e} = ECS.spawnEntity w0
      {world: w2, entity: e'} = addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e w1
      {world: w3, entity: _} = addComponent (Proxy :: _ "velocity") {x: 1.0, y: 1.0} e' w2
  pure w3

-- 3. Create system using State monad
moveSystem :: Number -> System (position :: Position, velocity :: Velocity) (position :: Position) Unit
moveSystem dt = do
  results <- S.query $ query {position: {x:0.0, y:0.0}, velocity: {x:0.0, y:0.0}}
  for_ results \r -> do
    let newPos = {x: r.components.position.x + r.components.velocity.x * dt
                 ,y: r.components.position.y + r.components.velocity.y * dt}
    void $ updateComponent (Proxy :: _ "position") newPos r.entity

-- 4. Run game loop
main = do
  world <- setupWorld
  let {world: world', result: _} = runSystem (moveSystem 0.016) world
  -- All entities moved!
```

---

**Last Updated**: 2025-10-15 (State Monad Migration)
**Version**: 2.0.0
**Status**: Production Ready ✅

## Migration from 1.x to 2.0

**Breaking Change**: Old `mkSystem`/`composeSystem` API removed in favor of State monad.

### Old API (1.x):
```purescript
import ECS.System (mkSystem, composeSystem, queryInSystem)

mySystem = mkSystem \world ->
  let results = runQuery q world
      update r w = ...
  in {world: foldl update world results, result: unit}

composed = composeSystem sys1 sys2
```

### New API (2.0):
```purescript
import ECS.System (System, runSystem, updateComponent)
import ECS.System as S

mySystem :: System (position :: Position) (position :: Position) Unit
mySystem = do
  results <- S.query $ query (Proxy :: _ (position :: Position))
  for_ results \r -> do
    void $ updateComponent (Proxy :: _ "position") newPos r.entity

-- Composition via do-notation
composed = do
  sys1
  sys2
```

### Migration Steps:
1. Replace `mkSystem \world -> {world, result}` with `do` notation or `state \w -> Tuple result w`
2. Replace `runQuery q world` with `results <- S.query $ query (Proxy :: _ (...))`
3. Replace `composeSystem sys1 sys2` with `do { sys1; sys2 }`
4. Add qualified import: `import ECS.System as S`
5. For inline systems, use: `import Control.Monad.State as CMS` and `CMS.state \w -> Tuple result w'`
