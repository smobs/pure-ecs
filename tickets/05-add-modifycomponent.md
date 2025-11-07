# Issue #5: Add `modifyComponent` Helper

**Priority**: 🟡 Medium
**Effort**: 1 hour
**Impact**: Medium - Dramatically improves common operation

## Problem

Updating a component with a function requires verbose code:

```purescript
maybePos <- getComponent (Proxy :: _ "position") entity
case maybePos of
  Just pos -> updateComponent (Proxy :: _ "position") (pos { x = pos.x + 1.0 }) entity
  Nothing -> pure entity
```

This is **extremely verbose** for a very common operation (read-modify-write).

## Solution

Add a helper that combines get + modify + update:

```purescript
modifyComponent :: forall label a r r' writes trash.
  IsSymbol label =>
  Cons label a r' r =>
  Cons label a trash writes =>
  Lacks label r' =>
  Proxy label ->
  (a -> a) ->
  Entity r ->
  System r' writes (Entity r)
modifyComponent proxy f entity = state \world ->
  case getComponentPure proxy entity world of
    Nothing -> Tuple entity world
    Just value ->
      let {world: w1, entity: e1} = removeComponentPure proxy entity world
          {world: w2, entity: e2} = addComponentPure proxy (f value) e1 w1
      in Tuple e2 w2
```

Also add underscore variant:

```purescript
modifyComponent_ :: forall label a r r' writes trash.
  IsSymbol label =>
  Cons label a r' r =>
  Cons label a trash writes =>
  Lacks label r' =>
  Proxy label ->
  (a -> a) ->
  Entity r ->
  System r' writes Unit
modifyComponent_ proxy f entity = void $ modifyComponent proxy f entity
```

## Usage

```purescript
-- Before (verbose)
maybePos <- getComponent (Proxy :: _ "position") entity
case maybePos of
  Just pos -> updateComponent (Proxy :: _ "position") (pos { x = pos.x + 1.0 }) entity
  Nothing -> pure entity

-- After (concise)
modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 1.0 }) entity

-- Even better with record update syntax
modifyComponent (Proxy :: _ "position") (_ { x = _ + 1.0 }) entity
```

## Implementation Steps

1. **Add functions to `src/ECS/System.purs`**
   - Implement `modifyComponent`
   - Implement `modifyComponent_`
   - Add comprehensive documentation

2. **Export from module**
   - Add to export list
   - Update module header documentation

3. **Update CLAUDE.md**
   - Add to "Common Patterns" section
   - Show examples with record updates
   - Document when to use vs `updateComponent`

4. **Consider using in examples**
   - Update SimpleExample.purs to use it where appropriate
   - Show real-world usage

## Benefits

✅ Much more concise for read-modify-write
✅ Follows functional programming patterns (modify :: (a -> a) -> ...)
✅ Reduces boilerplate significantly
✅ Handles Maybe case automatically
✅ Type-safe - same constraints as updateComponent

## Files to Modify

- `src/ECS/System.purs` - Add both functions
- `CLAUDE.md` - Document pattern
- `src/ECS/Examples/SimpleExample.purs` - Use in physics system
- Export list in System module

## Testing

Add tests to `test/Main.purs`:

```purescript
describe "modifyComponent" do
  it "modifies an existing component" do
    let world = execState (do
          e <- spawnEntity
          e' <- addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
          pure e'
        ) emptyWorld

        -- Get entity from world
        {world: world', result: entity} = runSystem
          (modifyComponent (Proxy :: _ "position") (\p -> p {x = 10.0}))
          world

        maybePos = getComponentPure (Proxy :: _ "position") entity world'

    maybePos `shouldEqual` Just {x: 10.0, y: 0.0}

  it "handles missing component gracefully" do
    let world = execState spawnEntity emptyWorld
        {world: world', result: entity} = runSystem
          (modifyComponent (Proxy :: _ "position") (\p -> p {x = 10.0}))
          world

    -- Entity unchanged, world unchanged
    world' `shouldEqual` world
```

## Documentation

Add to CLAUDE.md under "Common Patterns":

```markdown
### Modifying Components

**Read-modify-write pattern**:
```purescript
-- Apply a function to a component value
modifyComponent (Proxy :: _ "position") (\pos -> pos { x = pos.x + velocity.x }) entity

-- With PureScript's record update syntax
modifyComponent (Proxy :: _ "position") (_ { x = _ + velocity.x }) entity

-- Fire-and-forget variant
modifyComponent_ (Proxy :: _ "health") (_ { current = _ - damage }) entity
```

**When to use**:
- Incrementing/decrementing values
- Updating record fields
- Applying transformations to components

**Comparison**:
| Operation | Use |
|-----------|-----|
| `updateComponent` | Setting a new value directly |
| `modifyComponent` | Transforming existing value |
| `getComponent` → `updateComponent` | When you need the old value elsewhere |
```

## Alternative Signatures (Consider)

### With explicit type parameter:
```purescript
-- If type inference struggles
modifyComponentTyped :: forall label a r r' writes trash.
  -- ... constraints ...
  Proxy label ->
  Proxy a ->  -- Explicit type witness
  (a -> a) ->
  Entity r ->
  System r' writes (Entity r)
```

### With default value:
```purescript
-- Modify or insert if missing
modifyOrInsert :: forall label a r r' writes trash.
  -- ... constraints ...
  Proxy label ->
  a ->        -- Default value if component missing
  (a -> a) ->
  Entity r ->
  System r' writes (Entity r)
```

## Success Criteria

- [ ] `modifyComponent` added to `ECS.System`
- [ ] `modifyComponent_` added to `ECS.System`
- [ ] Both exported from module
- [ ] Comprehensive documentation in CLAUDE.md
- [ ] Tests added and passing
- [ ] Used in at least one example (SimpleExample.purs)
