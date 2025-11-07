# ECS API Improvement Tickets

This folder contains detailed implementation tickets for improving the ECS API based on the architectural review conducted on 2025-11-07.

## Overview

The tickets address ergonomic improvements while maintaining the excellent type-safety architecture. The core design is production-ready; these tickets focus on developer experience and performance.

## Priority Tiers

### 🔴 Tier 1: Critical (Do First)

| Ticket | Title | Effort | Impact |
|--------|-------|--------|--------|
| [#01](01-type-directed-query-api.md) | Type-Directed Query API | 2 hours | High |
| [#04](04-fix-examples-monadic-api.md) | Fix Examples to Use Monadic API | 2 hours | High |

**Total Tier 1 effort**: ~4 hours

These tickets address the most significant usability issues:
- Query naming confusion (affects every system)
- Examples teaching wrong patterns (primary learning tool)

### 🟡 Tier 2: Important (Do Next)

| Ticket | Title | Effort | Impact |
|--------|-------|--------|--------|
| [#05](05-add-modifycomponent.md) | Add `modifyComponent` Helper | 1 hour | Medium |
| [#03](03-updatecomponent-underscore-variant.md) | Add `updateComponent_` Variant | 30 min | Low |
| [#06](06-optimize-archetype-storage.md) | Optimize Archetype Storage | 4 hours | Medium |

**Total Tier 2 effort**: ~5.5 hours

These improve common operations and performance:
- Read-modify-write operations become much cleaner
- Fire-and-forget updates reduce verbosity
- Performance optimization for archetype operations

### 🟢 Tier 3: Nice to Have

| Ticket | Title | Effort | Impact |
|--------|-------|--------|--------|
| [#07](07-add-rwsystem-alias.md) | Add RWSystem Type Alias | 15 min | Low |
| [#08](08-lens-integration.md) | Lens Integration | 3 hours | Low |

**Total Tier 3 effort**: ~3.25 hours

These are quality-of-life improvements for specific use cases:
- Type alias reduces verbosity for common pattern
- Lens integration for advanced users

## Issue Summary

### Issue #1: Type-Directed Query API ⭐ CRITICAL
**Problem**: Two `query` functions with same name cause confusion and require qualified imports.

**Solution**: Add `queryFor` function that uses type-directed approach:
```purescript
-- Clear, single function name
results <- queryFor @ (position :: Position, velocity :: Velocity)
```

### Issue #2: Proxy Syntax ⚠️ SKIPPED
**Note**: User requested to keep Proxy syntax as-is. No ticket created.

### Issue #3: updateComponent_ Variant
**Problem**: Must rebind entity even when type doesn't change.

**Solution**: Add fire-and-forget variant:
```purescript
updateComponent_ (Proxy :: _ "position") newPos entity
-- No rebinding needed
```

### Issue #4: Fix Examples ⭐ CRITICAL
**Problem**: SimpleExample.purs manually threads world instead of using monadic API.

**Solution**: Rewrite examples to use do-notation properly:
```purescript
physicsSystem dt = do
  results <- queryFor @ (...)
  for_ results \r -> do
    void $ updateComponent (Proxy :: _ "position") newPos r.entity
```

### Issue #5: modifyComponent Helper
**Problem**: Read-modify-write requires verbose case expression.

**Solution**: Add helper that combines get + modify + update:
```purescript
modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 1.0 }) entity
```

### Issue #6: Optimize Archetype Storage
**Problem**: String parsing and concatenation on every component operation.

**Solution**: Use structured type (Phase 1) then integer IDs (Phase 2):
```purescript
newtype ArchetypeId = ArchetypeId (Set String)
-- O(log n) set operations instead of O(n log n) sorting
```

### Issue #7: RWSystem Type Alias
**Problem**: Verbose when read and write sets are identical.

**Solution**: Add type alias for common case:
```purescript
type RWSystem rw a = System rw rw a
```

### Issue #8: Lens Integration
**Problem**: Component access could be more elegant for power users.

**Solution**: Create ECS.Lens module with lens combinators:
```purescript
entity ^. _position              -- Get
entity # _position .~ newPos     -- Set
entity # _position.x +~ 1.0      -- Modify field
```

## Implementation Order

**Recommended order** (respects dependencies):

1. **Week 1**: Tier 1 tickets (#01, #04)
   - Query API improvement
   - Fix examples
   - Impact: Major usability improvement

2. **Week 2**: Tier 2 helpers (#05, #03)
   - modifyComponent
   - updateComponent_
   - Impact: Cleaner daily usage

3. **Week 3**: Tier 2 performance (#06)
   - Archetype optimization
   - Impact: Better performance

4. **Week 4**: Tier 3 polish (#07, #08)
   - RWSystem alias
   - Lens integration (optional)
   - Impact: Quality of life

## Testing Strategy

Each ticket includes:
- ✅ Test cases to verify correctness
- ✅ Success criteria checklist
- ✅ Documentation updates
- ✅ Example usage

Run after each ticket:
```bash
spago test
spago run --main ECS.Examples.SimpleExample
```

## Documentation Updates

All tickets include updates to:
- `CLAUDE.md` - Main documentation
- Code examples
- Module documentation
- Migration notes where needed

## Architectural Principles (Maintained)

These tickets preserve the excellent core design:
- ✅ Row polymorphism for type safety
- ✅ Phantom types for zero-cost abstractions
- ✅ Pure functional architecture
- ✅ Smart constructors and internal module pattern
- ✅ State monad for composition

The improvements are **additive** - they add ergonomic helpers without changing core semantics.

## Questions?

For questions about any ticket, refer to:
- The ticket itself (detailed rationale and implementation)
- The architectural review document
- CLAUDE.md for current API patterns

## Status Tracking

Track implementation status here:

- [x] #01 - Type-Directed Query API
- [ ] #03 - updateComponent_ Variant
- [x] #04 - Fix Examples Monadic API
- [ ] #05 - modifyComponent Helper
- [ ] #06 - Optimize Archetype Storage
- [ ] #07 - RWSystem Type Alias
- [ ] #08 - Lens Integration

---

**Created**: 2025-11-07
**Review Grade**: A- (Excellent fundamentals, ergonomic refinement needed)
**Total Estimated Effort**: ~12.75 hours for all tickets
