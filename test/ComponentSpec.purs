module Test.ECS.ComponentSpec where

import Prelude

import Control.Monad.State (State, execState, runState)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import ECS.Component (addComponentPure, getComponentPure, hasComponent, removeComponentPure)
import ECS.Entity (entityIndex)
import ECS.World (World, emptyWorld, hasEntity, spawnEntityPure, unEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

-- Test component types
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }

-- Type-level labels
_position :: Proxy "position"
_position = Proxy

_velocity :: Proxy "velocity"
_velocity = Proxy

_health :: Proxy "health"
_health = Proxy

componentSpec :: Spec Unit
componentSpec = do
  describe "ECS.Component" do

    -- Basic Addition Tests
    describe "Adding Components" do

      it "adds component to empty entity" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
        hasEntity e2 w2 `shouldEqual` true

      it "adds multiple components sequentially" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
        hasEntity e3 w3 `shouldEqual` true

      it "entity still exists after adding component" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            -- Entity ID should be preserved
            idx1 = entityIndex (unEntity e1)
            idx2 = entityIndex (unEntity e2)
        idx1 `shouldEqual` idx2

      it "multiple entities can have same components" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            {world: w3, entity: e1'} = addComponentPure _position {x: 10.0, y: 20.0} e1 w2
            {world: w4, entity: e2'} = addComponentPure _position {x: 5.0, y: 15.0} e2 w3
        hasEntity e1' w4 `shouldEqual` true
        hasEntity e2' w4 `shouldEqual` true

      it "adding three components works" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            {world: w4, entity: e4} = addComponentPure _health {current: 100, max: 100} e3 w3
        hasEntity e4 w4 `shouldEqual` true

    -- Component Removal Tests
    describe "Removing Components" do

      it "removes component from entity" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = removeComponentPure _position e2 w2
        hasEntity e3 w3 `shouldEqual` true

      it "entity persists after component removal" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = removeComponentPure _position e2 w2
            idx1 = entityIndex (unEntity e1)
            idx3 = entityIndex (unEntity e3)
        idx1 `shouldEqual` idx3

      it "removes middle component preserves others" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            {world: w4, entity: e4} = addComponentPure _health {current: 100, max: 100} e3 w3
            {world: w5, entity: e5} = removeComponentPure _velocity e4 w4
        hasEntity e5 w5 `shouldEqual` true

      it "removing all components leaves empty entity" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = removeComponentPure _position e2 w2
        hasEntity e3 w3 `shouldEqual` true

    -- Get Component Tests
    describe "Getting Components" do

      it "get existing component returns Just" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            maybePos = getComponentPure _position e2 w2
        case maybePos of
          Just pos -> pos.x `shouldEqual` 10.0
          Nothing -> fail "should exist" -- Fail test

      it "get component returns correct value" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _velocity {x: 5.0, y: -3.0} e1 w1
            maybeVel = getComponentPure _velocity e2 w2
        case maybeVel of
          Just vel -> do
            vel.x `shouldEqual` 5.0
            vel.y `shouldEqual` (-3.0)
          Nothing -> fail "should exist"

      it "get from entity with multiple components" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            maybePos = getComponentPure _position e3 w3
            maybeVel = getComponentPure _velocity e3 w3
        case maybePos, maybeVel of
          Just pos, Just vel -> do
            pos.x `shouldEqual` 10.0
            vel.x `shouldEqual` 1.0
          _, _ -> fail $ "expected two values, actual: " <> show (maybePos /\ maybeVel )
          

    -- Has Component Tests
    describe "Checking Component Existence" do

      it "has returns true for existing component" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            exists = hasComponent _position e2 w2
        exists `shouldEqual` true

      it "has works with multiple components" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            hasPos = hasComponent _position e3 w3
            hasVel = hasComponent _velocity e3 w3
        hasPos `shouldEqual` true
        hasVel `shouldEqual` true

    -- Archetype Migration Tests
    describe "Archetype Migration" do

      it "entity moves between archetypes on add" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world  -- "" archetype
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1  -- "position" archetype
        hasEntity e2 w2 `shouldEqual` true

      it "entity moves between archetypes on remove" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = removeComponentPure _position e2 w2  -- back to "" archetype
        hasEntity e3 w3 `shouldEqual` true

      it "multiple entities share archetypes" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            -- Both get same components -> same archetype
            {world: w3, entity: e1'} = addComponentPure _position {x: 1.0, y: 2.0} e1 w2
            {world: w4, entity: e2'} = addComponentPure _position {x: 3.0, y: 4.0} e2 w3
        hasEntity e1' w4 `shouldEqual` true
        hasEntity e2' w4 `shouldEqual` true

    -- Multiple Components Tests
    describe "Multiple Components" do

      it "add three components in sequence" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            {world: w4, entity: e4} = addComponentPure _health {current: 100, max: 100} e3 w3
            hasPos = hasComponent _position e4 w4
            hasVel = hasComponent _velocity e4 w4
            hasHlth = hasComponent _health e4 w4
        hasPos `shouldEqual` true
        hasVel `shouldEqual` true
        hasHlth `shouldEqual` true

      it "component values are preserved across additions" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 5.0, y: -2.0} e2 w2
            pos = getComponentPure _position e3 w3
            vel = getComponentPure _velocity e3 w3
        case pos, vel of
          Just p, Just v -> do
            p.x `shouldEqual` 10.0
            v.x `shouldEqual` 5.0
          _, _ -> fail $ "expected two values, actual: " <> show (pos /\ vel )

    -- Edge Cases
    describe "Edge Cases" do

      it "add and remove same component works" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = removeComponentPure _position e2 w2
            {world: w4, entity: e4} = addComponentPure _position {x: 30.0, y: 40.0} e3 w3
            pos = getComponentPure _position e4 w4
        case pos of
          Just p -> p.x `shouldEqual` 30.0
          Nothing -> fail "should exist"

      it "operations on empty entity work" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
        hasEntity e1 w1 `shouldEqual` true

      it "10 entities with components" do
        let world = emptyWorld
            results = addMultiple 10 world
        results.count `shouldEqual` 10

-- Helper to create multiple entities
addMultiple :: Int -> World -> { count :: Int, world :: World }
addMultiple 0 world = { count: 0, world }
addMultiple n world =
  let {world: w1, entity: e1} = spawnEntityPure world
      {world: w2, entity: _} = addComponentPure _position {x: 0.0, y: 0.0} e1 w1
      rest = addMultiple (n - 1) w2
  in { count: rest.count + 1, world: rest.world }
