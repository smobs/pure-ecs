-- | Web Demo: Visual ECS Example with Canvas Rendering
-- |
-- | This module creates an interactive web visualization of the ECS system.
-- | It extends the SimpleExample to render entities as colored circles on
-- | an HTML5 canvas, with visual feedback for movement, health, and damage.
module ECS.Examples.WebDemo where

import Prelude

import Control.Monad.State (State, execState)
import Data.Array (filter, length)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import ECS.Component ((<+>), (:=))
import ECS.Query (query, runQuery)
import ECS.System (System, runSystem, queryFor, updateComponent)
import ECS.World (World, emptyWorld, spawnEntity, despawnEntity)
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- ============================================================================
-- Component Definitions
-- ============================================================================

type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }
type Damage = { amount :: Int }
type Visual = { color :: String, radius :: Number }
type DamageFlash = { ticksRemaining :: Int }  -- Visual feedback for damage

-- ============================================================================
-- Canvas Rendering (Foreign Interface)
-- ============================================================================

foreign import data CanvasRenderingContext2D :: Type
foreign import data Ref :: Type -> Type

foreign import clearCanvas :: CanvasRenderingContext2D -> Number -> Number -> Effect Unit
foreign import drawCircle :: CanvasRenderingContext2D -> Number -> Number -> Number -> String -> Effect Unit
foreign import drawText :: CanvasRenderingContext2D -> String -> Number -> Number -> String -> Effect Unit
foreign import fillBackground :: CanvasRenderingContext2D -> String -> Number -> Number -> Effect Unit
foreign import drawRect :: CanvasRenderingContext2D -> Number -> Number -> Number -> Number -> String -> Effect Unit
foreign import requestAnimationFrame :: (Number -> Effect Unit) -> Effect Unit
foreign import createRef :: forall a. a -> Effect (Ref a)
foreign import readRef :: forall a. Ref a -> Effect a
foreign import writeRef :: forall a. Ref a -> a -> Effect Unit
foreign import getCanvasContext :: Effect CanvasRenderingContext2D
foreign import setControlCallbacks :: (Boolean -> Effect Unit) -> Effect Unit -> (Int -> Effect Unit) -> Effect Unit

-- ============================================================================
-- Systems
-- ============================================================================

physicsSystem :: forall w r. Number -> System (position :: Position, velocity :: Velocity | r)
                                   (position :: Position, velocity :: Velocity | w)
                                   Int
physicsSystem dt = do
  -- Query for all moving entities
  results <- queryFor @(position :: Position, velocity :: Velocity)

  -- Update each entity's position and handle bouncing
  for_ results \r -> do
    let newPos = { x: r.components.position.x + r.components.velocity.x * dt
                 , y: r.components.position.y + r.components.velocity.y * dt
                 }
        -- Keep entities on screen (bounce)
        bounceX = if newPos.x < 20.0 || newPos.x > 580.0
                  then { x: -r.components.velocity.x, y: r.components.velocity.y }
                  else r.components.velocity
        bounceY = if newPos.y < 20.0 || newPos.y > 380.0
                  then { x: bounceX.x, y: -r.components.velocity.y }
                  else bounceX
        clampedPos = { x: clamp 20.0 580.0 newPos.x
                     , y: clamp 20.0 380.0 newPos.y
                     }

    entity' <- updateComponent (Proxy :: _ "position") clampedPos r.entity
    when (bounceX /= r.components.velocity || bounceY /= r.components.velocity) do
      void $ updateComponent (Proxy :: _ "velocity") bounceY entity'

  pure $ length results
  where
    clamp min max val = if val < min then min else if val > max then max else val

damageSystem :: forall w r. System (health :: Health, damage :: Damage | r)
                       (health :: Health | w)
                       Int
damageSystem = do
  -- Query for all entities that can take damage
  results <- queryFor @(health :: Health, damage :: Damage)

  -- Count entities that died this tick (before applying damage)
  let deadCount = length $ filter (\r -> r.components.health.current <= r.components.damage.amount) results

  -- Apply damage to each entity
  for_ results \r -> do
    let newHealth = r.components.health.current - r.components.damage.amount
        updatedHealth = r.components.health { current = newHealth }
    void $ updateComponent (Proxy :: _ "health") updatedHealth r.entity

  pure deadCount

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
-- Rendering
-- ============================================================================

renderWorld :: CanvasRenderingContext2D -> World -> Effect Unit
renderWorld ctx world = do
  -- Clear canvas
  clearCanvas ctx 600.0 400.0
  fillBackground ctx "#1a1a2e" 600.0 400.0

  -- Query all visible entities
  let q = query (Proxy :: _ (position :: Position, visual :: Visual, health :: Health))
      results = runQuery q world

  -- Render each entity
  for_ results \r -> do
    let pos = r.components.position
        vis = r.components.visual
        hp = r.components.health
        healthPct = toNumber hp.current / toNumber hp.max

    -- Add glow effect for low health
    when (healthPct < 0.3) do
      drawCircle ctx pos.x pos.y (vis.radius + 8.0) "#ff000033"  -- Red glow
    when (healthPct < 0.5 && healthPct >= 0.3) do
      drawCircle ctx pos.x pos.y (vis.radius + 5.0) "#ffaa0033"  -- Orange glow

    -- Draw entity circle
    drawCircle ctx pos.x pos.y vis.radius vis.color

    -- Draw white outline for damaged entities
    when (healthPct < 1.0) do
      drawCircle ctx pos.x pos.y (vis.radius + 2.0) "#ffffff33"

    -- Draw health bar background (larger and more visible)
    let barWidth = vis.radius * 2.4
        barHeight = 6.0
        barX = pos.x - (barWidth / 2.0)
        barY = pos.y - vis.radius - 14.0

    -- Black background with white border
    drawRect ctx (barX - 1.0) (barY - 1.0) (barWidth + 2.0) (barHeight + 2.0) "#ffffff"
    drawRect ctx barX barY barWidth barHeight "#1a1a2e"

    -- Draw health bar fill with color based on health
    let healthWidth = barWidth * healthPct
        healthColor = if healthPct > 0.6
                      then "#2ecc71"  -- Green
                      else if healthPct > 0.3
                      then "#f39c12"  -- Orange
                      else "#e74c3c"  -- Red
    when (healthWidth > 0.0) do
      drawRect ctx barX barY healthWidth barHeight healthColor

  pure unit

-- ============================================================================
-- World Setup
-- ============================================================================

setupWorld :: World
setupWorld = execState setupEntities emptyWorld
  where
    setupEntities :: State World Unit
    setupEntities = do
      -- Entity 1: Red ball with damage (will die slowly)
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 100.0, y: 100.0 }
        <+> (Proxy :: _ "velocity") := { x: 50.0, y: 30.0 }
        <+> (Proxy :: _ "health") := { current: 100, max: 100 }
        <+> (Proxy :: _ "damage") := { amount: 2 }
        <+> (Proxy :: _ "visual") := { color: "#e74c3c", radius: 15.0 }

      -- Entity 2: Green ball, no damage (survives)
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 300.0, y: 200.0 }
        <+> (Proxy :: _ "velocity") := { x: -30.0, y: 40.0 }
        <+> (Proxy :: _ "health") := { current: 100, max: 100 }
        <+> (Proxy :: _ "visual") := { color: "#2ecc71", radius: 20.0 }

      -- Entity 3: Blue ball with moderate damage
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 500.0, y: 150.0 }
        <+> (Proxy :: _ "velocity") := { x: -40.0, y: -35.0 }
        <+> (Proxy :: _ "health") := { current: 100, max: 100 }
        <+> (Proxy :: _ "damage") := { amount: 3 }
        <+> (Proxy :: _ "visual") := { color: "#3498db", radius: 12.0 }

      -- Entity 4: Yellow ball, fast, no damage
      void $ spawnEntity
        <+> (Proxy :: _ "position") := { x: 200.0, y: 300.0 }
        <+> (Proxy :: _ "velocity") := { x: 60.0, y: -50.0 }
        <+> (Proxy :: _ "health") := { current: 100, max: 100 }
        <+> (Proxy :: _ "visual") := { color: "#f39c12", radius: 18.0 }

-- ============================================================================
-- Main Loop
-- ============================================================================

type GameState =
  { world :: World
  , tickCount :: Int
  , paused :: Boolean
  , frameCount :: Int
  , framesPerUpdate :: Int
  }

initialState :: GameState
initialState =
  { world: setupWorld
  , tickCount: 0
  , paused: false
  , frameCount: 0
  , framesPerUpdate: 30  -- Update every 30 frames = 2 updates per second at 60 FPS
  }

gameTick :: Number -> World -> { world :: World, stats :: { moved :: Int, killed :: Int, cleaned :: Int } }
gameTick dt world =
  let {world: world1, result} = runSystem (do
       moved <- physicsSystem dt
       killed <- damageSystem
       cleaned <- cleanupSystem
       pure {moved, killed, cleaned}) world
  in { world: world1, stats: result }

countEntities :: World -> Int
countEntities world =
  let q = query (Proxy :: _ (position :: Position))
      results = runQuery q world
  in length results

-- Initialize and run the demo
main :: Effect Unit
main = do
  log "Starting Pure ECS Web Demo..."

  -- Get canvas context from JavaScript
  ctx <- getCanvasContext

  log "Canvas initialized, starting animation loop..."

  -- Create mutable reference to game state
  stateRef <- createRef initialState

  -- Setup control callbacks
  let pauseCallback paused = do
        state <- readRef stateRef
        writeRef stateRef (state { paused = paused })

      resetCallback = do
        writeRef stateRef initialState

      setSpeedCallback framesPerUpdate = do
        state <- readRef stateRef
        writeRef stateRef (state { framesPerUpdate = framesPerUpdate })

  setControlCallbacks pauseCallback resetCallback setSpeedCallback

  let loop timestamp = do
        state <- readRef stateRef

        unless state.paused do
          -- Increment frame counter
          let newFrameCount = state.frameCount + 1
              shouldUpdate = newFrameCount >= state.framesPerUpdate

          -- Only update game state every N frames
          let {world: newWorld, stats, newTick} =
                if shouldUpdate
                  then let {world: w, stats: s} = gameTick 1.0 state.world
                       in {world: w, stats: s, newTick: state.tickCount + 1}
                  else {world: state.world, stats: {moved: 0, killed: 0, cleaned: 0}, newTick: state.tickCount}

              newState = state
                { world = newWorld
                , tickCount = newTick
                , frameCount = if shouldUpdate then 0 else newFrameCount
                }

          -- Render every frame for smooth animation
          renderWorld ctx newWorld

          -- Draw stats panel with background
          drawRect ctx 5.0 5.0 200.0 130.0 "#000000aa"
          drawText ctx ("=== ECS SIMULATION ===") 15.0 25.0 "#ffffff"
          drawText ctx ("Tick: " <> show newState.tickCount) 15.0 45.0 "#00ff00"
          drawText ctx ("Entities: " <> show (countEntities newWorld)) 15.0 65.0 "#00ff00"
          drawText ctx ("") 15.0 85.0 "#ffffff"
          drawText ctx ("Systems this tick:") 15.0 85.0 "#ffaa00"
          drawText ctx ("  Physics: " <> show stats.moved <> " updated") 15.0 100.0 "#ffffff"
          drawText ctx ("  Damage: " <> show stats.killed <> " damaged") 15.0 115.0 "#ffffff"
          drawText ctx ("  Cleanup: " <> show stats.cleaned <> " removed") 15.0 130.0 "#ffffff"

          -- Save state
          writeRef stateRef newState

        -- Request next frame
        requestAnimationFrame loop

  requestAnimationFrame loop
