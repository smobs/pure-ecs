# Issue #1: Type-Directed Query API

**Priority**: 🔴 High
**Effort**: 2 hours
**Impact**: High - Affects every system

## Problem

Current API has confusing naming where two different `query` functions exist:
```purescript
results <- S.query $ query (Proxy :: _ (position :: Position))
--         ^^^^^^     ^^^^^
--         Different functions with same name!
```

This requires qualified imports and causes cognitive load for users.

## Solution: Type-Directed Approach

Instead of renaming functions, use **type annotations** to guide which function is called:

```purescript
-- In ECS.System module
queryFor :: forall required excluded reads writes extra rl.
  RowToList required rl =>
  ExtractLabels rl =>
  ReadComponents rl required =>
  Union required extra reads =>
  Proxy required ->
  System reads writes (Array (QueryResult required))
queryFor proxy = query (ECS.Query.query proxy)
```

### Usage Pattern

```purescript
-- Before (confusing)
results <- S.query $ query (Proxy :: _ (position :: Position, velocity :: Velocity))

-- After (type-directed, clear)
results <- queryFor @ (position :: Position, velocity :: Velocity)
--         ^^^^^^^^^
--         Single clear function name, type-directed
```

### Alternative: Explicit Type Annotation

If visible type applications aren't preferred:
```purescript
results <- queryFor (Proxy :: _ (position :: Position, velocity :: Velocity))
```

This keeps both `query` functions but provides a cleaner entry point.

## Implementation Steps

1. **Add `queryFor` to `ECS.System`**
   - Takes Proxy with component row type
   - Internally calls both query functions
   - Single import for users

2. **Update documentation**
   - CLAUDE.md examples use `queryFor`
   - Mark bare `query` as advanced usage
   - Update Quick Start section

3. **Update examples**
   - SimpleExample.purs
   - WebDemo.purs
   - All inline examples in CLAUDE.md

4. **Add migration note**
   - Document pattern in "Best Practices"
   - Explain when to use qualified imports

## Benefits

✅ Single function name for users
✅ Type-directed, follows PureScript conventions
✅ Backwards compatible (old API still works)
✅ No qualified imports needed for common case
✅ Clearer intent in code

## Files to Modify

- `src/ECS/System.purs` - Add `queryFor` function
- `src/ECS/Examples/SimpleExample.purs` - Update to use `queryFor`
- `src/ECS/Examples/WebDemo.purs` - Update to use `queryFor`
- `CLAUDE.md` - Update all examples and Quick Start
- `test/Main.purs` - Update test examples if needed

## Testing

- All existing tests should pass
- System type inference should work correctly
- Type errors should be clear when component types mismatch

## Documentation Updates

Add to CLAUDE.md under "Common Patterns":

```markdown
### Querying Entities

**Recommended pattern** (type-directed):
```purescript
results <- queryFor @ (position :: Position, velocity :: Velocity)
```

**Advanced pattern** (explicit):
```purescript
import ECS.Query as Q
results <- S.query $ Q.query (Proxy :: _ (position :: Position))
```
```

## Success Criteria

- [ ] `queryFor` function added to `ECS.System`
- [ ] All examples updated to use new pattern
- [ ] CLAUDE.md documentation updated
- [ ] Tests pass
- [ ] No breaking changes to existing API
