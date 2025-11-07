# Issue #6: Optimize Archetype Storage

**Priority**: 🟡 Medium
**Effort**: 4 hours
**Impact**: Medium - Performance and safety improvements

## Problem

Current `ArchetypeId` is a string with comma-separated component names:

```purescript
type ArchetypeId = String  -- "Position,Velocity"

parseArchetypeId :: ArchetypeId -> Set String
parseArchetypeId archId =
  if archId == "" then Set.empty
  else Set.fromFoldable $ map trim $ split (Pattern ",") archId
```

**Issues**:
- O(n log n) string sorting on every component add/remove
- String parsing overhead on queries
- Case-sensitive, prone to typos
- String concatenation allocations
- No compile-time validation

## Solution (Phase 1): Structured Type

Replace String with a proper data structure:

```purescript
-- New type (internally stored, maintains invariants)
newtype ArchetypeId = ArchetypeId (Set String)

derive newtype instance eqArchetypeId :: Eq ArchetypeId
derive newtype instance ordArchetypeId :: Ord ArchetypeId

-- Smart constructors maintain sorted invariant
emptyArchetype :: ArchetypeId
emptyArchetype = ArchetypeId Set.empty

addComponentToArchetype :: String -> ArchetypeId -> ArchetypeId
addComponentToArchetype label (ArchetypeId set) =
  ArchetypeId (Set.insert label set)

removeComponentFromArchetype :: String -> ArchetypeId -> ArchetypeId
removeComponentFromArchetype label (ArchetypeId set) =
  ArchetypeId (Set.delete label set)

-- For Map keys, we need a String representation
archetypeIdToKey :: ArchetypeId -> String
archetypeIdToKey (ArchetypeId set) =
  joinWith "," (Set.toUnfoldable set :: Array String)
```

## Solution (Phase 2): Integer IDs (Future)

For maximum performance:

```purescript
type ArchetypeId = Int
type ArchetypeRegistry = Map (Set String) Int

-- World would include
type World =
  { entities :: EntityManager
  , archetypes :: Map ArchetypeId Archetype
  , archetypeRegistry :: ArchetypeRegistry  -- Component signature → ID
  , nextArchetypeId :: Int
  , entityLocations :: Map Int ArchetypeId
  }
```

**Benefits**:
- O(1) comparison
- No string operations
- Enables bitset matching in future
- Compact memory representation

## Implementation Steps (Phase 1)

1. **Define new ArchetypeId type**
   - Create in `src/ECS/World.purs`
   - Smart constructors
   - Conversion functions

2. **Update World module**
   - Replace String operations with Set operations
   - Update `archetypeMatches` in Query module
   - Remove string parsing functions

3. **Update Component module**
   - Replace `addLabelToArchetype` implementation
   - Replace `removeLabelFromArchetype` implementation
   - Use Set operations instead of string concatenation

4. **Update Query module**
   - `archetypeMatches` uses Set directly
   - No parsing needed

5. **Backward compatibility**
   - Keep string serialization for debugging
   - Add `Show` instance for pretty printing

6. **Performance testing**
   - Benchmark before/after
   - Profile archetype operations
   - Measure query performance

## Benefits

✅ No string parsing overhead
✅ O(1) set operations instead of O(n log n) sorting
✅ Type-safe (newtype wrapper)
✅ Eliminates string allocation churn
✅ Foundation for future bitset optimization
✅ Cleaner code (no manual parsing)

## Files to Modify

- `src/ECS/World.purs` - New ArchetypeId type
- `src/ECS/Component.purs` - Update archetype operations
- `src/ECS/Query.purs` - Update matching logic
- `test/Main.purs` - Verify behavior unchanged

## Testing

Key test cases:
- Archetype creation and lookup
- Component add/remove updates archetype correctly
- Query matching works with new format
- Entity migration between archetypes
- Empty archetype handling
- Performance benchmarks

## Performance Expectations

| Operation | Before | After (Phase 1) | After (Phase 2) |
|-----------|--------|-----------------|-----------------|
| Add component | O(n log n) | O(log n) | O(log n) |
| Query matching | O(n) parse + O(m) | O(m) | O(1) bitset |
| Archetype comparison | O(n) string | O(1) set | O(1) int |

Where:
- n = number of components in signature
- m = size of required/excluded sets

## Migration Notes

This is an **internal change** - external API remains the same. Users should see:
- Better performance
- No code changes needed
- Identical behavior

## Phase 2 Planning (Future Ticket)

For integer IDs:
- Add ArchetypeRegistry to World
- Generate unique IDs on archetype creation
- Update Map keys from String to Int
- Consider bitset encoding for ultra-fast queries

## Success Criteria

Phase 1:
- [ ] ArchetypeId is newtype over Set String
- [ ] No string parsing in hot paths
- [ ] All tests pass
- [ ] Performance improvement measurable
- [ ] No API changes for users

Phase 2 (future):
- [ ] Integer-based IDs
- [ ] Registry implementation
- [ ] Bitset matching (optional)

## Code Example

### Before
```purescript
addLabelToArchetype :: String -> ArchetypeId -> ArchetypeId
addLabelToArchetype label archId =
  let labels = parseArchetypeId archId  -- Parse string
      newLabels = label : labels
  in buildArchetypeId newLabels         -- Re-sort and concatenate
```

### After (Phase 1)
```purescript
addLabelToArchetype :: String -> ArchetypeId -> ArchetypeId
addLabelToArchetype label (ArchetypeId set) =
  ArchetypeId (Set.insert label set)  -- O(log n), no parsing
```

### After (Phase 2)
```purescript
addComponentToEntity :: ... -> { world :: World, archetypeId :: ArchetypeId }
addComponentToEntity label entity world =
  let signature = currentSignature world entity
      newSignature = Set.insert label signature
  in case Map.lookup newSignature world.archetypeRegistry of
       Just id -> { world, archetypeId: id }
       Nothing ->
         let newId = world.nextArchetypeId
             registry' = Map.insert newSignature newId world.archetypeRegistry
         in { world: world { archetypeRegistry = registry'
                           , nextArchetypeId = newId + 1 }
            , archetypeId: newId }
```
