# Issue #4: Fix Examples to Use Monadic API Properly

**Priority**: 🔴 High
**Effort**: 2 hours
**Impact**: High - Examples are primary learning tool

## Problem

`SimpleExample.purs` doesn't properly use the monadic System API. Current code:

```purescript
physicsSystem dt = state \world ->
  let results = runQuery q world
      updatePosition result w =
        let {world: w', result: _} = runSystem (updateComponent ...) w
        in w'
      world' = foldl (\w r -> updatePosition r w) world results
  in Tuple (...) world'
```

This **manually threads world** through `foldl` and calls `runSystem` repeatedly. The System monad isn't being leveraged!

## Solution

Use do-notation properly with `for_` over query results:

```purescript
physicsSystem :: forall w r. Number -> System (position :: Position, velocity :: Velocity | r)
                                   (position :: Position | w)
                                   Int
physicsSystem dt = do
  results <- queryFor (Proxy :: _ (position :: Position, velocity :: Velocity))
  for_ results \r -> do
    let newPos = { x: r.components.position.x + r.components.velocity.x * dt
                 , y: r.components.position.y + r.components.velocity.y * dt }
    void $ updateComponent (Proxy :: _ "position") newPos r.entity
  pure $ length results
```

## Why This Matters

**Current approach teaches wrong pattern**:
- Users see manual world threading
- Nested `runSystem` calls are confusing
- Defeats purpose of State monad abstraction

**Correct approach shows**:
- Clean do-notation
- No explicit world threading
- Composable systems
- Idiomatic PureScript

## Implementation Steps

1. **Fix `physicsSystem` in SimpleExample.purs**
   - Use do-notation
   - Use `for_` over results
   - Remove manual `foldl` and `runSystem` calls

2. **Fix `damageSystem`**
   - Same pattern

3. **Fix `cleanupSystem`**
   - Same pattern

4. **Update `gameTick`**
   - Show proper system composition with do-notation
   - Demonstrate result aggregation

5. **Update CLAUDE.md**
   - "Writing Systems" section
   - Remove or clarify "manual state pattern"
   - Emphasize do-notation as primary approach

6. **Add anti-pattern note**
   - Document when manual `state \w -> ...` is appropriate (rarely)
   - Show correct pattern for 99% of cases

## Before/After Examples

### Before (Manual Threading)
```purescript
physicsSystem dt = state \world ->
  let results = runQuery q world
      update result w = runSystem (updateComponent ...) w |> _.world
      world' = foldl (\w r -> update r w) world results
  in Tuple (length results) world'
```

### After (Monadic)
```purescript
physicsSystem dt = do
  results <- queryFor @ (position :: Position, velocity :: Velocity)
  for_ results \r -> do
    void $ updateComponent (Proxy :: _ "position") (computeNewPos r) r.entity
  pure $ length results
```

## Files to Modify

- `src/ECS/Examples/SimpleExample.purs` - Fix all three systems
- `CLAUDE.md` - Update "Writing Systems" section
- `CLAUDE.md` - Update Quick Start if needed
- `docs/` - Any tutorials that show system patterns

## Testing

- Run `spago test` to ensure no breakage
- Run `runExample` to verify behavior is identical
- Visually inspect output logs

## Documentation Updates

### CLAUDE.md "Writing Systems" Section

**Remove**:
```markdown
-- Manual state pattern (for inline systems)
simpleSystem = CMS.state \world -> ...
```

**Replace with**:
```markdown
### Writing Systems (Do-Notation)

**Standard pattern** (use this 99% of the time):
```purescript
mySystem :: System reads writes a
mySystem = do
  results <- queryFor @ (...)
  for_ results \r -> do
    -- Update components
    void $ updateComponent (Proxy :: _ "label") newValue r.entity
  pure someResult
```

**Advanced pattern** (only when you need manual control):
```purescript
-- Rarely needed - avoid unless you have a specific reason
mySystem = state \world ->
  let result = -- complex computation
  in Tuple result world'
```
```

## Benefits

✅ Examples teach correct patterns
✅ Code is more readable
✅ Demonstrates monadic composition
✅ Aligns with PureScript conventions
✅ Easier for newcomers to understand

## Success Criteria

- [ ] All three systems in SimpleExample.purs use do-notation
- [ ] No manual world threading via `foldl`
- [ ] No nested `runSystem` calls
- [ ] CLAUDE.md updated to emphasize monadic pattern
- [ ] Tests pass
- [ ] Example output unchanged
