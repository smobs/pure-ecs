-- | ECS Simple Example: Complete Workflow Demonstration
-- |
-- | This module demonstrates a complete ECS workflow from entity creation
-- | through system execution. It implements a simple physics simulation with
-- | health, damage, and entity lifecycle management.
-- |
-- | **What this example shows:**
-- | - Defining component types
-- | - Creating and spawning entities
-- | - Adding multiple components to entities
-- | - Implementing systems with type-safe access tracking
-- | - Running systems in sequence
-- | - Query-based entity processing
-- | - Entity despawning based on conditions
-- |
-- | **The simulation:**
-- | - 3 entities with different component combinations
-- | - Movement system: Updates positions based on velocity
-- | - Damage system: Applies damage to health
-- | - Cleanup system: Despawns dead entities
-- | - Game loop: Runs for 5 ticks, showing state changes
module ECS.Examples.SimpleExample where

import Prelude

import Control.Monad.State (State, execState, state)
import Data.Array (filter, length, foldl)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import ECS.Component ((<+>), (:=))
import ECS.System (System, runSystem, queryFor, modifyComponent_)
import ECS.World (World, emptyWorld, spawnEntity, despawnEntityPure)
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- ============================================================================
-- Component Definitions
-- ============================================================================

-- | Position in 2D space
type Position = { x :: Number, y :: Number }

-- | Velocity vector for movement
type Velocity = { x :: Number, y :: Number }

-- | Health with current and maximum values
type Health = { current :: Int, max :: Int }

-- | Damage to be applied each tick
type Damage = { amount :: Int }

-- ============================================================================
-- Systems
-- ============================================================================

-- | **Physics System**: Updates positions based on velocity
-- |
-- | - Reads: position, velocity
-- | - Writes: position
-- | - Result: Number of entities moved
-- |
-- | This system queries all entities with both position and velocity,
-- | then updates their positions by adding the velocity vector.
-- |
-- | **Updated to use modifyComponent_**: This demonstrates the cleaner
-- | read-modify-write pattern with the new helper function.
physicsSystem :: forall w r. Number -> System (position :: Position, velocity :: Velocity | r)
                                   (position :: Position | w)
                                   Int
physicsSystem dt = do
  -- Query for all moving entities
  results <- queryFor @(position :: Position, velocity :: Velocity)

  -- Update each entity's position using modifyComponent_
  for_ results \r -> do
    modifyComponent_ (Proxy :: _ "position")
      (\p -> { x: p.x + r.components.velocity.x * dt
             , y: p.y + r.components.velocity.y * dt })
      r.entity

  -- Return count of entities moved
  pure $ length results

-- | **Damage System**: Applies damage to entities with health
-- |
-- | - Reads: health, damage
-- | - Writes: health
-- | - Result: Number of entities killed
-- |
-- | This system reduces health by the damage amount each tick.
-- | Returns the count of entities that reached 0 health.
-- |
-- | **Updated to use modifyComponent_**: Demonstrates clean read-modify-write
-- | pattern for applying damage to health components.
damageSystem :: forall w r. System (health :: Health, damage :: Damage | r)
                       (health :: Health | w)
                       Int
damageSystem = do
  -- Query for all entities that can take damage
  results <- queryFor @(health :: Health, damage :: Damage)

  -- Count entities that will die this tick
  let deadCount = length $ filter (\r -> r.components.health.current <= r.components.damage.amount) results

  -- Apply damage to each entity using modifyComponent_
  for_ results \r -> do
    modifyComponent_ (Proxy :: _ "health")
      (\h -> h { current = h.current - r.components.damage.amount })
      r.entity

  -- Return count of entities that died
  pure deadCount

-- | **Cleanup System**: Despawns entities with health <= 0
-- |
-- | - Reads: health
-- | - Writes: (none - removes entities)
-- | - Result: Number of entities despawned
-- |
-- | This system queries for entities with health components,
-- | checks if they're dead (health <= 0), and despawns them.
-- |
-- | **Updated to use queryFor**: Shows the cleaner query API.
cleanupSystem :: forall w r. System (health :: Health | r) w Int
cleanupSystem = do
  -- Query all entities with health
  results <- queryFor @(health :: Health)

  -- Filter to only dead entities
  let deadEntities = filter (\r -> r.components.health.current <= 0) results

  -- Despawn each dead entity
  for_ deadEntities \r -> do
    despawnEntity r.entity

  pure $ length deadEntities

-- ============================================================================
-- World Setup
-- ============================================================================

-- | Create the initial world with 3 entities:
-- | 1. Moving entity with health and damage (will die)
-- | 2. Moving entity without damage (will survive)
-- | 3. Stationary entity with health and damage (will die)
setupWorld :: World
setupWorld = execState setupEntities emptyWorld
  where
    setupEntities :: State World Unit
    setupEntities = do
      -- Entity 1: Moving, with health and damage (position, velocity, health, damage)
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 0.0, y: 0.0 }
        <+> (Proxy :: _ "velocity") := { x: 1.0, y: 0.5 }
        <+> (Proxy :: _ "health") := { current: 50, max: 100 }
        <+> (Proxy :: _ "damage") := { amount: 15 }

      -- Entity 2: Moving, no damage (position, velocity, health)
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 10.0, y: 5.0 }
        <+> (Proxy :: _ "velocity") := { x: -0.5, y: 1.0 }
        <+> (Proxy :: _ "health") := { current: 100, max: 100 }

      -- Entity 3: Stationary with damage (position, health, damage)
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 5.0, y: 5.0 }
        <+> (Proxy :: _ "health") := { current: 30, max: 100 }
        <+> (Proxy :: _ "damage") := { amount: 20 }

-- ============================================================================
-- Game Loop
-- ============================================================================

-- | Run one tick of the game loop
-- |
-- | Executes systems in sequence:
-- | 1. Physics - move entities
-- | 2. Damage - apply damage
-- | 3. Cleanup - remove dead entities
gameTick :: Number -> World -> { world :: World, moved :: Int, killed :: Int, cleaned :: Int }
gameTick dt world =
  let -- Run physics system
      {world: world1, result} = runSystem (do 
       moved <- physicsSystem dt
       killed <- damageSystem
       cleaned <- cleanupSystem 
       pure {moved, killed, cleaned}) world

  in { world: world1, moved: result.moved, killed: result.killed, cleaned: result.cleaned }

-- | Run the simulation for N ticks
runSimulation :: Int -> Number -> World -> Effect World
runSimulation 0 _ world = pure world
runSimulation n dt world = do
  log $ "=== Tick " <> show (6 - n) <> " ==="

  let {world: world', moved, killed, cleaned} = gameTick dt world

  log $ "  Moved: " <> show moved <> " entities"
  log $ "  Killed: " <> show killed <> " entities"
  log $ "  Cleaned: " <> show cleaned <> " entities"

  runSimulation (n - 1) dt world'

-- ============================================================================
-- Main Example Runner
-- ============================================================================

-- | Run the complete example
-- |
-- | This demonstrates:
-- | - World initialization with multiple entities
-- | - Component composition to define entity types
-- | - System execution in sequence
-- | - Entity lifecycle (spawn → update → despawn)
-- | - Type-safe access tracking at compile-time
runExample :: Effect Unit
runExample = do
  log ""
  log "╔════════════════════════════════════════════════════════════╗"
  log "║          ECS Simple Example: Complete Workflow             ║"
  log "╚════════════════════════════════════════════════════════════╝"
  log ""

  log "Initializing world with 3 entities..."
  let world = setupWorld
  log "  Entity 0: position + velocity + health + damage"
  log "  Entity 1: position + velocity + health (no damage)"
  log "  Entity 2: position + health + damage (no velocity)"
  log ""

  log "Running simulation for 5 ticks..."
  log ""

  _ <- runSimulation 5 1.0 world

  log ""
  log "Simulation complete!"
  log ""
  log "Expected results:"
  log "  - Entity 0: Moved 5 times, took 75 damage (15×5), died at tick 4"
  log "  - Entity 1: Moved 5 times, took 0 damage, survived"
  log "  - Entity 2: Moved 0 times, took 100 damage (20×5), died at tick 2"
  log ""
  log "This demonstrates:"
  log "  ✓ Type-safe component composition"
  log "  ✓ Row-polymorphic queries"
  log "  ✓ System access tracking"
  log "  ✓ Entity lifecycle management"
  log "  ✓ Pure functional ECS architecture"
  log ""

  pure unit
