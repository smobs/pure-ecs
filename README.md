# pure-ecs

A pure functional Entity Component System (ECS) for PureScript.

## Overview

`pure-ecs` is a high-performance, type-safe ECS implementation leveraging PureScript's advanced type system. It uses archetype-based storage for cache-friendly iteration and row polymorphism for compile-time component safety.

## Features

- **Pure Functional**: Immutable world state, enabling time-travel debugging and easy testing
- **Type-Safe**: Row polymorphism with `Lacks` and `Cons` constraints prevent bugs at compile-time
- **High Performance**: Archetype-based storage groups entities by component signature for cache-friendly iteration
- **Generational Indices**: Prevents use-after-free bugs through entity versioning
- **Ergonomic**: Clean State monad API with do-notation for system composition
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
import Effect (Effect)
import Effect.Console (log)
import ECS.World (emptyWorld, spawnEntity)
import ECS.Component (addComponent)
import ECS.Query (query, runQuery)
import ECS.System (runSystem)
import Type.Proxy (Proxy(..))

-- Define components
type Position = { x :: Number, y :: Number }
type Velocity = { dx :: Number, dy :: Number }

-- Create entities
createEntity :: System _ _ (Entity (position :: Position, velocity :: Velocity))
createEntity = do
  entity <- spawnEntity
  entity' <- addComponent (Proxy :: _ "position") { x: 0.0, y: 0.0 } entity
  addComponent (Proxy :: _ "velocity") { dx: 1.0, dy: 0.5 } entity'

-- Query and update
physicsSystem :: System (position :: Position, velocity :: Velocity) (position :: Position) Unit
physicsSystem = do
  results <- query (Proxy :: _ (position :: Position, velocity :: Velocity))
  forQuery results \{ entity, components: { position, velocity } } ->
    updateComponent (Proxy :: _ "position")
      { x: position.x + velocity.dx, y: position.y + velocity.dy }
      entity

main :: Effect Unit
main = do
  let world = emptyWorld
  let world' = runSystem createEntity world
  let world'' = runSystem physicsSystem world'
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
