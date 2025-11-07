# Issue #8: Lens Integration

**Priority**: 🟢 Low
**Effort**: 3 hours
**Impact**: Low - Advanced feature for power users

## Problem

Component access could be more elegant with lens support:

```purescript
-- Current approach
maybePos <- getComponent (Proxy :: _ "position") entity
entity' <- updateComponent (Proxy :: _ "position") newPos entity

-- Desired lens approach
entity ^. _position              -- Get
entity # _position .~ newPos     -- Set
entity # _position.x +~ 1.0      -- Modify field
```

PureScript has `purescript-profunctor-lenses` for this pattern.

## Solution

Create a new module `ECS.Lens` with lens combinators:

```purescript
module ECS.Lens
  ( _component
  , componentLens
  ) where

import Data.Lens (Lens', lens)
import ECS.Component (getComponentPure, updateComponent)
import ECS.World (Entity)
import Type.Proxy (Proxy)
import Type.Data.Symbol (class IsSymbol)

-- | Create a lens for a component.
-- |
-- | Note: This only works within a System context since we need World access.
-- | For pure lens operations, use in combination with State monad.
componentLens :: forall label a r r' trash.
  IsSymbol label =>
  Cons label a trash r =>
  Cons label a r' r =>
  Lacks label r' =>
  Proxy label ->
  -- Returns a lens-like structure for use in System monad
  ComponentLens label a r

-- Helper type for component access
data ComponentLens label a r = ComponentLens (Proxy label)

-- Lens-style get
(^.) :: forall label a r trash.
  IsSymbol label =>
  Cons label a trash r =>
  Entity r ->
  ComponentLens label a r ->
  System r () (Maybe a)
entity ^. ComponentLens proxy = do
  world <- get
  pure $ getComponentPure proxy entity world

-- Lens-style set
(.~) :: forall label a r r' trash writes.
  IsSymbol label =>
  Cons label a r' r =>
  Cons label a trash writes =>
  Lacks label r' =>
  ComponentLens label a r ->
  a ->
  Entity r ->
  System r' writes (Entity r)
ComponentLens proxy .~ value = \entity ->
  updateComponent proxy value entity

-- Lens-style modify
(%~) :: forall label a r r' trash writes.
  IsSymbol label =>
  Cons label a r' r =>
  Cons label a trash writes =>
  Lacks label r' =>
  ComponentLens label a r ->
  (a -> a) ->
  Entity r ->
  System r' writes (Entity r)
ComponentLens proxy %~ f = \entity ->
  modifyComponent proxy f entity
```

## Usage Examples

```purescript
import ECS.Lens (_component)

-- Define lens helpers (users could generate these)
_position :: ComponentLens "position" Position
_position = _component (Proxy :: _ "position")

_velocity :: ComponentLens "velocity" Velocity
_velocity = _component (Proxy :: _ "velocity")

-- In a system
mySystem :: RWSystem (position :: Position, velocity :: Velocity) Unit
mySystem = do
  results <- queryFor @ (position :: Position, velocity :: Velocity)
  for_ results \r -> do
    -- Lens-style get
    maybePos <- r.entity ^. _position

    -- Lens-style set
    void $ r.entity # _position .~ {x: 0.0, y: 0.0}

    -- Lens-style modify
    void $ r.entity # _position %~ (\p -> p {x = p.x + 1.0})

    -- Compose with record lenses for field access
    void $ r.entity # _position.x +~ 1.0  -- (requires full lens integration)
```

## Advanced: Full Lens Integration

For true lens composition with record fields:

```purescript
-- This requires wrapping Entity + World in a structure that lenses can traverse
data EntityView r = EntityView (Entity r) World

_componentField :: forall label a r r' trash field fieldType.
  IsSymbol label =>
  Cons label a trash r =>
  Cons label a r' r =>
  Lacks label r' =>
  HasField field a fieldType =>
  Proxy label ->
  Proxy field ->
  Lens' (EntityView r) fieldType
_componentField labelProxy fieldProxy = lens getter setter
  where
    getter (EntityView entity world) =
      case getComponentPure labelProxy entity world of
        Just component -> Record.get fieldProxy component
        Nothing -> unsafeCrashWith "Component not found"

    setter (EntityView entity world) newValue =
      case getComponentPure labelProxy entity world of
        Just component ->
          let updated = Record.set fieldProxy newValue component
              {world: w', entity: e'} = updateComponentPure labelProxy updated entity world
          in EntityView e' w'
        Nothing -> EntityView entity world

-- Usage
entity # _position.x .~ 10.0
entity # _position.x +~ 5.0
```

## Implementation Steps

1. **Create new module `src/ECS/Lens.purs`**
   - Start with simple ComponentLens type
   - Implement basic operators: `^.`, `.~`, `%~`
   - Add documentation and examples

2. **Add dependency**
   - Add `purescript-profunctor-lenses` to spago.yaml
   - Update package.json if needed

3. **Create examples**
   - `src/ECS/Examples/LensExample.purs`
   - Show component access patterns
   - Compare to non-lens approach

4. **Update CLAUDE.md**
   - Add "Advanced: Lens Integration" section
   - Mark as optional/advanced
   - Show both approaches

5. **Testing**
   - Add tests for lens operations
   - Verify type inference
   - Test composition

## Benefits

✅ Elegant syntax for power users
✅ Composable with other lenses
✅ Familiar pattern from Haskell ecosystem
✅ Enables point-free style
✅ Field-level updates possible

## Drawbacks

⚠️ Adds dependency
⚠️ Learning curve for lens beginners
⚠️ May be overkill for simple use cases
⚠️ Type errors can be complex

## When to Use

**Use lenses when**:
- You're doing complex nested updates
- You want point-free composition
- You're familiar with lens libraries
- You need field-level access patterns

**Stick with basic API when**:
- Simple get/update operations
- Learning the ECS library
- Avoiding dependencies
- Prioritizing clear error messages

## Files to Create/Modify

- `src/ECS/Lens.purs` - New module
- `src/ECS/Examples/LensExample.purs` - New example
- `spago.yaml` - Add lens dependency
- `CLAUDE.md` - Document advanced feature
- `test/Main.purs` - Add lens tests

## Testing

```purescript
describe "ECS.Lens" do
  it "lens get retrieves component" do
    let world = execState (do
          e <- spawnEntity
          addComponent (Proxy :: _ "position") {x: 5.0, y: 10.0} e
        ) emptyWorld

    -- Test lens get
    let {result: maybePos} = runSystem (entity ^. _position) world
    maybePos `shouldEqual` Just {x: 5.0, y: 10.0}

  it "lens set updates component" do
    let world = execState (do
          e <- spawnEntity
          addComponent (Proxy :: _ "position") {x: 0.0, y: 0.0} e
        ) emptyWorld

    -- Test lens set
    let {world: w', result: e'} = runSystem
          (entity # _position .~ {x: 10.0, y: 20.0})
          world

    let maybePos = getComponentPure (Proxy :: _ "position") e' w'
    maybePos `shouldEqual` Just {x: 10.0, y: 20.0}
```

## Documentation

Add to CLAUDE.md:

```markdown
## Advanced: Lens Integration

For power users familiar with lens libraries, ECS provides lens-based component access:

```purescript
import ECS.Lens (_component)

-- Define component lenses
_position = _component (Proxy :: _ "position")
_velocity = _component (Proxy :: _ "velocity")

mySystem = do
  results <- queryFor @ (position :: Position)
  for_ results \r -> do
    -- Get component
    maybePos <- r.entity ^. _position

    -- Set component
    void $ r.entity # _position .~ newPos

    -- Modify component
    void $ r.entity # _position %~ updateFunction
```

**Requirements**:
- `purescript-profunctor-lenses` dependency
- Familiarity with lens patterns

**When to use**: Complex nested updates, point-free composition, field-level access
**When to avoid**: Learning ECS, simple operations, minimizing dependencies
```

## Alternative: Row-Typed Lenses

Consider creating lenses that work at the row level:

```purescript
type EntityLens r = Lens' World (Record r)

-- Provides lens into entity's component record
entityLens :: forall r. Entity r -> EntityLens r
```

This could enable even more powerful composition but requires deeper integration.

## Success Criteria

- [ ] `ECS.Lens` module created
- [ ] Basic lens operators implemented (`^.`, `.~`, `%~`)
- [ ] Example showing lens usage
- [ ] Tests passing
- [ ] Documented in CLAUDE.md as advanced feature
- [ ] Marked as optional (not required for basic usage)
