# pure-ecs

A pure functional Entity Component System (ECS) for PureScript.

## 🎮 [**Live Demo →**](https://smobs.github.io/pure-ecs/)

See the ECS in action with an interactive visual demonstration!

## Overview

`pure-ecs` is a high-performance, type-safe ECS implementation leveraging PureScript's advanced type system. It uses archetype-based storage for cache-friendly iteration and row polymorphism for compile-time component safety.

## Features

- **Pure Functional**: Immutable world state, enabling time-travel debugging and easy testing
- **Type-Safe**: Row polymorphism with `Lacks` and `Cons` constraints prevent bugs at compile-time
- **Chaining Combinator**: Prevents stale entity reference bugs - impossible to use wrong intermediate entity!
- **Monadic API**: Clean State monad API eliminates manual world threading - no more `{world: w1, entity: e1}` patterns!
- **High Performance**: Archetype-based storage groups entities by component signature for cache-friendly iteration
- **Generational Indices**: Prevents use-after-free bugs through entity versioning
- **Self-Documenting**: Compose systems into a `Pipeline` and generate markdown docs (with mermaid data-flow diagrams) from the same value used to run the game — drift is impossible
- **Production Ready**: Comprehensive test coverage (150+ tests)

### Self-documenting games

Compose your systems into a `Pipeline` and pure-ecs will generate markdown
docs of your game's structure — execution order, system reads/writes, and
inter-system data flow as a mermaid diagram. The pipeline value used to
**run** your game is the same value used to **document** it; the two
cannot drift. See [`docs/example-pipeline.md`](docs/example-pipeline.md)
for a real generated doc.

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
import Control.Monad.State (execState)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import ECS.World (World, emptyWorld, spawnEntity)
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, updateComponent, queryFor)
import Type.Proxy (Proxy(..))

-- Define components
type Position = { x :: Number, y :: Number }
type Velocity = { dx :: Number, dy :: Number }

-- Create entities using chaining combinator (impossible to use stale reference!)
setupWorld =
  void $ spawnEntity
    <+> (Proxy :: _ "position") := { x: 0.0, y: 0.0 }
    <+> (Proxy :: _ "velocity") := { dx: 1.0, dy: 0.5 }

-- Query and update system
physicsSystem :: System (position :: Position, velocity :: Velocity) (position :: Position) Unit
physicsSystem = do
  results <- queryFor @(position :: Position, velocity :: Velocity)
  for_ results \r -> do
    let newPos = { x: r.components.position.x + r.components.velocity.dx
                 , y: r.components.position.y + r.components.velocity.dy }
    updateComponent_ (Proxy :: _ "position") newPos r.entity

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
