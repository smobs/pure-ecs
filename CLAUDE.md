# ECS Implementation Guide

Pure functional Entity Component System with row-polymorphic type safety.

## Quick Start

```purescript
import Control.Monad.State (execState)
import ECS.World (emptyWorld, spawnEntity)
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, updateComponent, queryFor)
import Type.Proxy (Proxy(..))

-- Define components
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }

-- Create world and spawn entity using chaining combinator (impossible to use stale reference!)
let world = execState (
      spawnEntity
        <+> (Proxy :: _ "position") := {x: 0.0, y: 0.0}
        <+> (Proxy :: _ "velocity") := {x: 1.0, y: 1.0}
      -- Entity type: Entity (position :: Position, velocity :: Velocity)
    ) emptyWorld

-- Create system using State monad
    moveSystem :: System (position :: Position, velocity :: Velocity) (position :: Position) Unit
    moveSystem = do
      results <- queryFor @(position :: Position, velocity :: Velocity)
      for_ results \r -> do
        void $ updateComponent (Proxy :: _ "position") newPos r.entity
      -- Type-safe access: r.components.position

-- Run system
    let {world: world', result: _} = runSystem moveSystem world
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
- **Chaining combinator** (`<+>`) prevents stale entity reference bugs

**Key functions**: `addComponent`, `removeComponent`, `getComponent`, `hasComponent`, `<+>` (with)

**Important**: `addComponent` cannot update existing components (has `Lacks` constraint).
Use `removeComponent` then `addComponent`, or use System's `updateComponent` helper.

**Chaining Pattern**: Use the `<+>` combinator to add components without manual entity threading:
```purescript
entity <- spawnEntity
  <+> (Proxy :: _ "position") := {x: 0.0, y: 0.0}
  <+> (Proxy :: _ "velocity") := {x: 1.0, y: 1.0}
-- Impossible to accidentally use wrong intermediate entity!
```

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

**Key functions**: `runSystem`, `queryFor`, `updateComponent`

**API Pattern**: Systems are State monad computations that query entities and update components:
```purescript
-- Run system with runSystem
let {world: w', result: value} = runSystem mySystem world

-- Query within system (recommended)
results <- queryFor @(position :: Position, velocity :: Velocity)

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
-- Chaining combinator prevents stale entity reference bugs
buildEntity =
  spawnEntity
    <+> (Proxy :: _ "position") := pos
    <+> (Proxy :: _ "velocity") := vel
    <+> (Proxy :: _ "health") := hp
  -- Result type: Entity (position :: Position, velocity :: Velocity, health :: Health)
  -- Impossible to accidentally use wrong intermediate entity!

-- Execute within a world
let world' = execState buildEntity emptyWorld
```

### Writing Systems
```purescript
-- State monad approach (current API)
import ECS.System (System, runSystem, updateComponent, queryFor)

movementSystem :: Number -> System (position :: Position, velocity :: Velocity)
                                     (position :: Position)
                                     Int
movementSystem dt = do
  results <- queryFor @(position :: Position, velocity :: Velocity)
  for_ results \r -> do
    let newPos = {x: r.components.position.x + r.components.velocity.x * dt
                 ,y: r.components.position.y + r.components.velocity.y * dt}
    void $ updateComponent (Proxy :: _ "position") newPos r.entity
  pure $ length results

-- Manual state pattern (for inline systems)
import Control.Monad.State as CMS
import ECS.Query (query, runQuery)

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
import ECS.System (System, runSystem, updateComponent, queryFor)
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
import Control.Monad.State (execState)
import ECS.World as ECS
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, updateComponent, queryFor)
import Type.Proxy (Proxy(..))
import Data.Traversable (for_)

-- 1. Define components
type Position = {x :: Number, y :: Number}
type Velocity = {x :: Number, y :: Number}

-- 2. Setup world (chaining combinator - impossible to use stale reference!)
setupWorld = execState setupEntities ECS.emptyWorld
  where
    setupEntities =
      ECS.spawnEntity
        <+> (Proxy :: _ "position") := {x: 0.0, y: 0.0}
        <+> (Proxy :: _ "velocity") := {x: 1.0, y: 1.0}

-- 3. Create system using State monad
moveSystem :: Number -> System (position :: Position, velocity :: Velocity) (position :: Position) Unit
moveSystem dt = do
  results <- queryFor @(position :: Position, velocity :: Velocity)
  for_ results \r -> do
    let newPos = {x: r.components.position.x + r.components.velocity.x * dt
                 ,y: r.components.position.y + r.components.velocity.y * dt}
    void $ updateComponent (Proxy :: _ "position") newPos r.entity

-- 4. Run game loop
main = do
  let world = setupWorld
      {world: world', result: _} = runSystem (moveSystem 0.016) world
  -- All entities moved!
```

---

**Last Updated**: 2025-11-07 (Chaining Combinator)
**Version**: 3.1.0
**Status**: Production Ready ✅

## Migration from 3.0 to 3.1

**Major Improvement**: Chaining combinator (`<+>`) prevents stale entity reference bugs by making it impossible to use wrong intermediate entities.

### Old API (3.0 - manual entity threading):
```purescript
let world' = execState (do
      e1 <- spawnEntity
      e2 <- addComponent (Proxy :: _ "position") pos e1
      e3 <- addComponent (Proxy :: _ "velocity") vel e2  -- Easy to accidentally use e1 instead!
      pure e3
    ) world
```

### New API (3.1 - chaining combinator):
```purescript
let world' = execState (
      spawnEntity
        <+> (Proxy :: _ "position") := pos
        <+> (Proxy :: _ "velocity") := vel
      -- Impossible to use wrong entity reference!
    ) world
```

### Migration Steps:
1. Import `(<+>)` and `(:=)` from `ECS.Component`
2. Replace manual entity threading (`e1 <- ...; e2 <- addComponent ... e1`) with chaining
3. Wrap component label and value in `(Proxy :: _ "label") := value`
4. Remove intermediate entity bindings (e1, e2, e3, etc.)
5. Old API still available for cases requiring conditional logic

## Migration from 2.x to 3.0

**Major Improvement**: All World and Component operations now use monadic composition via the State monad, eliminating manual world threading.

### Old API (2.x):
```purescript
let {world: w1, entity: e1} = spawnEntity world
    {world: w2, entity: e2} = addComponent (Proxy :: _ "position") pos e1 w1
    {world: w3, entity: e3} = addComponent (Proxy :: _ "velocity") vel e2 w2
```

### New API (3.0+):
```purescript
let world' = execState (
      spawnEntity
        <+> (Proxy :: _ "position") := pos
        <+> (Proxy :: _ "velocity") := vel
    ) world
```

### Migration Steps:
1. Import `execState` from `Control.Monad.State`
2. Import `(<+>)` and `(:=)` from `ECS.Component`
3. Wrap entity creation code in `execState ... emptyWorld`
4. Use chaining combinator instead of manual threading
5. For tests or internal code, pure versions are available: `spawnEntityPure`, `addComponentPure`, etc.

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

### New API (2.0+):
```purescript
import ECS.System (System, runSystem, updateComponent, queryFor)

mySystem :: System (position :: Position) (position :: Position) Unit
mySystem = do
  results <- queryFor @(position :: Position)
  for_ results \r -> do
    void $ updateComponent (Proxy :: _ "position") newPos r.entity

-- Composition via do-notation
composed = do
  sys1
  sys2
```

### Migration Steps:
1. Replace `mkSystem \world -> {world, result}` with `do` notation or `state \w -> Tuple result w`
2. Replace `runQuery q world` with `results <- queryFor @(...)`
3. Replace `composeSystem sys1 sys2` with `do { sys1; sys2 }`
4. Import `queryFor` from `ECS.System`
5. For inline systems, use: `import Control.Monad.State as CMS` and `CMS.state \w -> Tuple result w'`
