-- | Web Demo: Visual ECS Example with Canvas Rendering
-- |
-- | This module creates an interactive web visualization of the ECS system.
-- | It extends the SimpleExample to render entities as colored circles on
-- | an HTML5 canvas, with visual feedback for movement, health, and damage.
module ECS.Examples.WebDemo where

import Prelude

import Control.Monad.State (State, state, execState)
import Data.Array (foldl, filter, length)
import Data.Int (toNumber)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import ECS.Component (addComponent)
import ECS.Query (query, runQuery)
import ECS.System (System, runSystem, updateComponent)
import ECS.World (World, emptyWorld, spawnEntity, despawnEntityPure)
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

-- ============================================================================
-- Systems
-- ============================================================================

physicsSystem :: forall w r. Number -> System (position :: Position, velocity :: Velocity | r)
                                   (position :: Position | w)
                                   Int
physicsSystem dt = state \world ->
  let q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
      results = runQuery q world

      updatePosition result w =
        let newPos = { x: result.components.position.x + result.components.velocity.x * dt
                     , y: result.components.position.y + result.components.velocity.y * dt
                     }
            -- Keep entities on screen (bounce)
            bounceX = if newPos.x < 20.0 || newPos.x > 580.0
                      then { x: -result.components.velocity.x, y: result.components.velocity.y }
                      else result.components.velocity
            bounceY = if newPos.y < 20.0 || newPos.y > 380.0
                      then { x: bounceX.x, y: -result.components.velocity.y }
                      else bounceX
            clampedPos = { x: clamp 20.0 580.0 newPos.x
                         , y: clamp 20.0 380.0 newPos.y
                         }
            {world: w', result: _} = runSystem (updateComponent (Proxy :: _ "position") clampedPos result.entity) w
            {world: w'', result: _} = if bounceX /= result.components.velocity || bounceY /= result.components.velocity
                                      then runSystem (updateComponent (Proxy :: _ "velocity") bounceY result.entity) w'
                                      else {world: w', result: result.entity}
        in w''

      world' = foldl (\w r -> updatePosition r w) world results
  in Tuple (foldl (\acc _ -> acc + 1) 0 results) world'
  where
    clamp min max val = if val < min then min else if val > max then max else val

damageSystem :: forall w r. System (health :: Health, damage :: Damage | r)
                       (health :: Health | w)
                       Int
damageSystem = state \world ->
  let q = query (Proxy :: _ (health :: Health, damage :: Damage))
      results = runQuery q world

      applyDamage result w =
        let newHealth = result.components.health.current - result.components.damage.amount
            updatedHealth = result.components.health { current = newHealth }
            {world: w', result: _} = runSystem (updateComponent (Proxy :: _ "health") updatedHealth result.entity) w
        in w'

      deadCount = foldl (\count r -> if r.components.health.current <= r.components.damage.amount then count + 1 else count) 0 results

      world' = foldl (\w r -> applyDamage r w) world results
  in Tuple deadCount world'

cleanupSystem :: forall w r. System (health :: Health | r) w Int
cleanupSystem = state \world ->
  let q = query (Proxy :: _ (health :: Health))
      results = runQuery q world
      deadEntities = filter (\r -> r.components.health.current <= 0) results
      despawnOne result w = despawnEntityPure result.entity w
      world' = foldl (\w r -> despawnOne r w) world deadEntities
  in Tuple (foldl (\acc _ -> acc + 1) 0 deadEntities) world'

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

    -- Draw entity circle
    drawCircle ctx pos.x pos.y vis.radius vis.color

    -- Draw health bar background
    let barWidth = vis.radius * 2.0
        barHeight = 4.0
        barX = pos.x - vis.radius
        barY = pos.y - vis.radius - 10.0

    drawRect ctx barX barY barWidth barHeight "#333333"

    -- Draw health bar fill
    let healthWidth = barWidth * healthPct
    drawRect ctx barX barY healthWidth barHeight "#2ecc71"

  pure unit

-- ============================================================================
-- World Setup
-- ============================================================================

setupWorld :: World
setupWorld = execState setupEntities emptyWorld
  where
    setupEntities :: State World Unit
    setupEntities = do
      -- Entity 1: Red ball with damage (will die)
      e1 <- spawnEntity
      e1' <- addComponent (Proxy :: _ "position") { x: 100.0, y: 100.0 } e1
      e1'' <- addComponent (Proxy :: _ "velocity") { x: 50.0, y: 30.0 } e1'
      e1''' <- addComponent (Proxy :: _ "health") { current: 50, max: 100 } e1''
      e1'''' <- addComponent (Proxy :: _ "damage") { amount: 15 } e1'''
      void $ addComponent (Proxy :: _ "visual") { color: "#e74c3c", radius: 15.0 } e1''''

      -- Entity 2: Green ball, no damage (survives)
      e2 <- spawnEntity
      e2' <- addComponent (Proxy :: _ "position") { x: 300.0, y: 200.0 } e2
      e2'' <- addComponent (Proxy :: _ "velocity") { x: -30.0, y: 40.0 } e2'
      e2''' <- addComponent (Proxy :: _ "health") { current: 100, max: 100 } e2''
      void $ addComponent (Proxy :: _ "visual") { color: "#2ecc71", radius: 20.0 } e2'''

      -- Entity 3: Blue ball, stationary with damage
      e3 <- spawnEntity
      e3' <- addComponent (Proxy :: _ "position") { x: 500.0, y: 150.0 } e3
      e3'' <- addComponent (Proxy :: _ "velocity") { x: -40.0, y: -35.0 } e3'
      e3''' <- addComponent (Proxy :: _ "health") { current: 30, max: 100 } e3''
      e3'''' <- addComponent (Proxy :: _ "damage") { amount: 20 } e3'''
      void $ addComponent (Proxy :: _ "visual") { color: "#3498db", radius: 12.0 } e3''''

      -- Entity 4: Yellow ball, fast
      e4 <- spawnEntity
      e4' <- addComponent (Proxy :: _ "position") { x: 200.0, y: 300.0 } e4
      e4'' <- addComponent (Proxy :: _ "velocity") { x: 60.0, y: -50.0 } e4'
      e4''' <- addComponent (Proxy :: _ "health") { current: 100, max: 100 } e4''
      void $ addComponent (Proxy :: _ "visual") { color: "#f39c12", radius: 18.0 } e4'''

-- ============================================================================
-- Main Loop
-- ============================================================================

type GameState =
  { world :: World
  , tickCount :: Int
  , paused :: Boolean
  }

initialState :: GameState
initialState =
  { world: setupWorld
  , tickCount: 0
  , paused: false
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

  let loop timestamp = do
        state <- readRef stateRef

        unless state.paused do
          -- Update game (60 FPS = ~16.67ms per frame, use 1.0 for easier visualization)
          let {world: newWorld, stats} = gameTick 1.0 state.world
              newState = state { world = newWorld, tickCount = state.tickCount + 1 }

          -- Render
          renderWorld ctx newWorld

          -- Draw stats
          drawText ctx ("Tick: " <> show newState.tickCount) 10.0 20.0 "#ffffff"
          drawText ctx ("Entities: " <> show (countEntities newWorld)) 10.0 40.0 "#ffffff"
          drawText ctx ("Moved: " <> show stats.moved) 10.0 60.0 "#ffffff"
          drawText ctx ("Cleaned: " <> show stats.cleaned) 10.0 80.0 "#ffffff"

          -- Save state
          writeRef stateRef newState

        -- Request next frame
        requestAnimationFrame loop

  requestAnimationFrame loop
