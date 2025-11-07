# Issue #3: Add `updateComponent_` Variant

**Priority**: 🟡 Medium
**Effort**: 30 minutes
**Impact**: Low - Quality of life improvement

## Problem

`updateComponent` requires rebinding even when entity type doesn't change:

```purescript
entity' <- updateComponent (Proxy :: _ "position") newPos entity
--^^^^^^ Must bind new variable even though ID is unchanged
```

This is verbose for same-type updates where you don't need the returned entity.

## Solution

Add a variant that returns `Unit` for fire-and-forget updates:

```purescript
updateComponent_ :: forall label a r r' writes trash.
  IsSymbol label =>
  Cons label a r' r =>
  Cons label a trash writes =>
  Lacks label r' =>
  Proxy label ->
  a ->
  Entity r ->
  System r' writes Unit
updateComponent_ proxy value entity = void $ updateComponent proxy value entity
```

## Usage

```purescript
-- When you don't need the entity back
updateComponent_ (Proxy :: _ "position") newPos entity
-- No rebinding needed

-- When you DO need it (original function still available)
entity' <- updateComponent (Proxy :: _ "position") newPos entity
```

## Implementation Steps

1. **Add function to `src/ECS/System.purs`**
   - Implement as `void $ updateComponent ...`
   - Add documentation explaining when to use each variant

2. **Export from module**
   - Add to export list
   - Document difference in module header

3. **Update CLAUDE.md**
   - Add to "Common Patterns" section
   - Show both variants and when to use each

4. **Update examples if beneficial**
   - Consider using in SimpleExample.purs where entity isn't reused

## Benefits

✅ Cleaner code for common case
✅ Signals intent (I don't need the entity back)
✅ Reduces visual noise
✅ No performance difference (just `void`)

## Files to Modify

- `src/ECS/System.purs` - Add function
- `CLAUDE.md` - Document pattern
- Optionally: examples if it improves readability

## Testing

- No new tests needed (it's just `void`)
- Existing tests cover underlying `updateComponent`

## Documentation

Add to CLAUDE.md under "Writing Systems":

```markdown
### Updating Components

**When you need the updated entity**:
```purescript
entity' <- updateComponent (Proxy :: _ "position") newPos entity
-- Use entity' in subsequent operations
```

**When you don't** (fire-and-forget):
```purescript
updateComponent_ (Proxy :: _ "position") newPos entity
-- Cleaner, signals intent
```
```

## Success Criteria

- [ ] Function added to `ECS.System`
- [ ] Exported from module
- [ ] Documented in CLAUDE.md
- [ ] Tests pass (no new tests needed)
