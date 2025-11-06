# Pure ECS Demo

This is the visual demo for the Pure ECS library.

## What you'll see

An interactive visualization of the ECS system showing:
- Colored balls representing entities
- Movement based on velocity components
- Health bars showing entity health
- Damage system that reduces health over time
- Cleanup system that removes dead entities

## How it works

The demo uses:
- **Position & Velocity components**: Entities move and bounce off walls
- **Health & Damage components**: Some entities take damage each tick
- **Visual component**: Defines color and size for rendering
- **Physics system**: Updates positions based on velocity
- **Damage system**: Applies damage to health
- **Cleanup system**: Removes entities when health reaches 0

## Color Legend

- **Red**: Takes damage (will die after a few seconds)
- **Green**: No damage (survives indefinitely)
- **Blue**: Takes damage (will die after a few seconds)
- **Yellow**: No damage (survives indefinitely)

## Building locally

To build and run the demo locally:

```bash
# Install dependencies
npm install

# Build the demo
npm run build:demo

# Serve locally
npm run serve

# Visit http://localhost:8000
```

## Learn more

- [GitHub Repository](https://github.com/smobs/pure-ecs)
- [Documentation](../README.md)
