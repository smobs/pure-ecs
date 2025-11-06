# pure-ecs

A pure functional Entity Component System (ECS) for PureScript.

## Overview

`pure-ecs` is a high-performance, type-safe ECS implementation leveraging PureScript's advanced type system. It uses archetype-based storage for cache-friendly iteration and row polymorphism for compile-time component safety.

## Features

- **Pure Functional**: Immutable world state, enabling time-travel debugging and easy testing
- **Type-Safe**: Row polymorphism with `Lacks` and `Cons` constraints prevent bugs at compile-time
- **Monadic API**: Clean State monad API eliminates manual world threading - no more `{world: w1, entity: e1}` patterns!
- **High Performance**: Archetype-based storage groups entities by component signature for cache-friendly iteration
- **Generational Indices**: Prevents use-after-free bugs through entity versioning
- **Production Ready**: Comprehensive test coverage (110+ tests)

## Installation

Add `pure-ecs` to your `spago.yaml`:

```yaml
package:
  dependencies:
    - pure-ecs
```

Then install:

```bash
spago install
```

## Quick Start

```purescript
module Main where

import Prelude
import Control.Monad.State (State, execState)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import ECS.World (World, emptyWorld, spawnEntity)
import ECS.Component (addComponent)
import ECS.Query (query)
import ECS.System (System, runSystem, updateComponent)
import ECS.System as S
import Type.Proxy (Proxy(..))

-- Define components
type Position = { x :: Number, y :: Number }
type Velocity = { dx :: Number, dy :: Number }

-- Create entities using monadic API (no manual world threading!)
setupWorld :: State World Unit
setupWorld = do
  entity <- spawnEntity
  entity' <- addComponent (Proxy :: _ "position") { x: 0.0, y: 0.0 } entity
  void $ addComponent (Proxy :: _ "velocity") { dx: 1.0, dy: 0.5 } entity'

-- Query and update system
physicsSystem :: System (position :: Position, velocity :: Velocity) (position :: Position) Unit
physicsSystem = do
  results <- S.query $ query (Proxy :: _ (position :: Position, velocity :: Velocity))
  for_ results \r -> do
    let newPos = { x: r.components.position.x + r.components.velocity.dx
                 , y: r.components.position.y + r.components.velocity.dy }
    void $ updateComponent (Proxy :: _ "position") newPos r.entity

main :: Effect Unit
main = do
  -- Build world with monadic composition
  let world = execState setupWorld emptyWorld
      {world: world', result: _} = runSystem physicsSystem world
  log "Simulation complete!"
```

## Documentation

For comprehensive documentation, see [CLAUDE.md](./CLAUDE.md), which covers:

- Architecture and design decisions
- Core module descriptions
- Common patterns
- Performance characteristics
- Troubleshooting guide

## Testing

Run the test suite:

```bash
spago test
```

## License

MIT License - see [LICENSE](./LICENSE) for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
