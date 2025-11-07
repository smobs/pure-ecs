# Issue #7: Add RWSystem Type Alias

**Priority**: 🟢 Low
**Effort**: 15 minutes
**Impact**: Low - Nice to have for common case

## Problem

Most systems read and write the same components:

```purescript
physicsSystem :: Number -> System (position :: Position, velocity :: Velocity)
                                   (position :: Position)
                                   Int
--                                 ^^^^^^^^^^^^^^^^^^^^^^
--                                 Often the same as read set
```

This is verbose when read and write sets are identical.

## Solution

Add a type alias for the common case where reads = writes:

```purescript
-- In ECS.System module
type RWSystem rw a = System rw rw a
```

## Usage

```purescript
-- Before (explicit read/write)
movementSystem :: Number -> System (position :: Position, velocity :: Velocity)
                                   (position :: Position, velocity :: Velocity)
                                   Unit

-- After (concise when read = write)
movementSystem :: Number -> RWSystem (position :: Position, velocity :: Velocity) Unit
```

## Implementation Steps

1. **Add type alias to `src/ECS/System.purs`**
   ```purescript
   -- | Type alias for systems where read and write sets are identical.
   -- |
   -- | Most systems read and write the same components, making this
   -- | a convenient shorthand.
   -- |
   -- | Example:
   -- | ```purescript
   -- | updatePositions :: RWSystem (position :: Position, velocity :: Velocity) Unit
   -- | ```
   type RWSystem rw a = System rw rw a
   ```

2. **Export from module**
   - Add to export list

3. **Update CLAUDE.md**
   - Document in "Writing Systems" section
   - Show when to use vs full System signature

4. **Optionally update examples**
   - Consider using where appropriate
   - Show both patterns

## Benefits

✅ Reduces verbosity for common case
✅ Signals intent (same read/write)
✅ Standard pattern in effect systems (RWS monad)
✅ No runtime cost (type alias only)

## When NOT to Use

Document cases where full System signature is better:

```purescript
-- ❌ DON'T use RWSystem when reads ≠ writes
readOnlySystem :: System (position :: Position) () Unit

-- ❌ DON'T use when it obscures the access pattern
complexSystem :: System (position :: Position, velocity :: Velocity, health :: Health)
                        (position :: Position, health :: Health)
                        Int

-- ✅ DO use when read = write and it's clear
simpleSystem :: RWSystem (position :: Position) Unit
```

## Files to Modify

- `src/ECS/System.purs` - Add type alias and documentation
- `CLAUDE.md` - Document usage pattern
- Optionally: examples if it improves clarity

## Testing

- No tests needed (type alias only)
- Verify type inference works correctly
- Check that error messages are still clear

## Documentation

Add to CLAUDE.md under "Writing Systems":

```markdown
### System Type Signatures

**When read and write sets are identical**:
```purescript
type RWSystem rw a = System rw rw a

-- Usage
updatePositions :: RWSystem (position :: Position, velocity :: Velocity) Unit
updatePositions = do
  -- Read and write the same components
  ...
```

**When read and write sets differ**:
```purescript
-- Be explicit
analyzeSystem :: System (position :: Position, velocity :: Velocity)  -- Reads
                        (diagnostics :: Diagnostics)                  -- Writes
                        AnalysisReport
```

**Read-only systems**:
```purescript
-- Write set is empty
countEntities :: System (position :: Position) () Int
```
```

## Success Criteria

- [ ] Type alias added to `ECS.System`
- [ ] Exported from module
- [ ] Documented in CLAUDE.md
- [ ] Shows when to use vs full signature
