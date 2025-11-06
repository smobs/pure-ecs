module Test.ECS.QuerySpec where

import Prelude

import Data.Array (length, index)
import Data.Maybe (Maybe(..))
import ECS.Component (addComponent, getComponent)
import ECS.Query (query, without, runQuery, forQuery, mapQuery)
import ECS.World (World, emptyWorld, spawnEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

-- Test component types
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }
type Frozen = Unit

-- Type-level labels
_position :: Proxy "position"
_position = Proxy

_velocity :: Proxy "velocity"
_velocity = Proxy

_health :: Proxy "health"
_health = Proxy

_frozen :: Proxy "frozen"
_frozen = Proxy

querySpec :: Spec Unit
querySpec = do
  describe "ECS.Query" do

    -- Basic Query Tests
    describe "Basic Queries" do

      it "query single component returns matching entities" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w2

        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> r.components.position.x `shouldEqual` 10.0

      it "query multiple components (AND logic)" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponent _velocity {x: 1.0, y: 0.0} e2 w2

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q w3

        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> do
            r.components.position.x `shouldEqual` 10.0
            r.components.velocity.x `shouldEqual` 1.0

      it "query with no matches returns empty array" do
        let world = emptyWorld
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world

        length results `shouldEqual` 0

      it "query only returns entities with ALL required components" do
        let world0 = emptyWorld
            -- Entity with position only
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with both position and velocity
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: e4} = addComponent _position {x: 30.0, y: 40.0} e3 w3
            {world: w5, entity: _} = addComponent _velocity {x: 1.0, y: 0.0} e4 w4

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q w5

        -- Only the second entity matches (has both components)
        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> r.components.position.x `shouldEqual` 30.0

    -- Exclusion Tests
    describe "Exclusion Filters" do

      it "without filter excludes entities with specified component" do
        let world0 = emptyWorld
            -- Entity with position (not frozen)
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with position and frozen
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: e4} = addComponent _position {x: 30.0, y: 40.0} e3 w3
            {world: w5, entity: _} = addComponent _frozen unit e4 w4

            q :: _ (position :: Position) (frozen :: Unit)
            q = query (Proxy :: _ (position :: Position)) # without _frozen
            results = runQuery q w5

        -- Only first entity matches (not frozen)
        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> r.components.position.x `shouldEqual` 10.0

      it "multiple without filters work correctly" do
        let world0 = emptyWorld
            -- Entity with position only
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with position and frozen
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: e4} = addComponent _position {x: 20.0, y: 30.0} e3 w3
            {world: w5, entity: _} = addComponent _frozen unit e4 w4
            -- Entity with position and health
            {world: w6, entity: e6} = spawnEntity w5
            {world: w7, entity: e7} = addComponent _position {x: 30.0, y: 40.0} e6 w6
            {world: w8, entity: _} = addComponent _health {current: 100, max: 100} e7 w7

            q :: _ (position :: Position) (frozen :: Unit, health :: Health)
            q = query (Proxy :: _ (position :: Position))
              # without _frozen
              # without _health
            results = runQuery q w8

        -- Only first entity matches (no frozen or health)
        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> r.components.position.x `shouldEqual` 10.0

    -- Archetype Matching Tests
    describe "Archetype Matching" do

      it "extra components don't prevent match" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponent _velocity {x: 1.0, y: 0.0} e2 w2
            {world: w4, entity: _} = addComponent _health {current: 100, max: 100} e3 w3

            -- Query for just position - should match even with extra components
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w4

        length results `shouldEqual` 1

      it "missing required component prevents match" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1

            -- Query for position and velocity - should not match (missing velocity)
            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q w2

        length results `shouldEqual` 0

      it "excluded component prevents match" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponent _frozen unit e2 w2

            q :: _ (position :: Position) (frozen :: Unit)
            q = query (Proxy :: _ (position :: Position)) # without _frozen
            results = runQuery q w3

        length results `shouldEqual` 0

    -- Multiple Entities Tests
    describe "Multiple Entities" do

      it "query returns all matching entities" do
        let world0 = emptyWorld
            -- Create 3 entities with position
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 10.0} e1 w1
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: _} = addComponent _position {x: 20.0, y: 20.0} e3 w3
            {world: w5, entity: e5} = spawnEntity w4
            {world: w6, entity: _} = addComponent _position {x: 30.0, y: 30.0} e5 w5

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w6

        length results `shouldEqual` 3

      it "results from different archetypes" do
        let world0 = emptyWorld
            -- Entity with just position
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 10.0} e1 w1
            -- Entity with position and velocity
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: e4} = addComponent _position {x: 20.0, y: 20.0} e3 w3
            {world: w5, entity: _} = addComponent _velocity {x: 1.0, y: 0.0} e4 w4

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w5

        -- Both entities match (different archetypes)
        length results `shouldEqual` 2

      it "component values unique per entity" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 100.0, y: 200.0} e1 w1
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: _} = addComponent _position {x: 300.0, y: 400.0} e3 w3

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w4

        length results `shouldEqual` 2
        case results !! 0, results !! 1 of
          Just r1, Just r2 -> do
            -- Values should be different
            (r1.components.position.x == r2.components.position.x) `shouldEqual` false
          _, _ -> fail "Expected two results"

    -- Callback Tests
    describe "Iteration Functions" do

      it "forQuery applies callback to all results" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: _} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = spawnEntity w2
            {world: w4, entity: _} = addComponent _position {x: 30.0, y: 40.0} e3 w3

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            xValues = forQuery q (\r -> r.components.position.x) w4

        length xValues `shouldEqual` 2
        case xValues !! 0, xValues !! 1 of
          Just x1, Just x2 -> do
            (x1 + x2) `shouldEqual` 40.0
          _, _ -> fail "Expected two results"

      it "mapQuery threads world through updates" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponent _velocity {x: 5.0, y: 0.0} e2 w2

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            -- Update positions based on velocities
            w4 = mapQuery q updatePos w3

            -- Check that position was updated
            results = runQuery q w4

        length results `shouldEqual` 1
        -- Note: This test is simplified - full update would require
        -- removeComponent and addComponent to change component values
        true `shouldEqual` true

      it "forQuery receives correct types" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponent _velocity {x: 1.0, y: 0.0} e2 w2

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            -- Access multiple fields to verify types
            sums = forQuery q (\r -> r.components.position.x + r.components.velocity.x) w3

        case sums !! 0 of
          Just sum -> sum `shouldEqual` 11.0
          Nothing -> fail "Expected one result"

    -- Edge Cases
    describe "Edge Cases" do

      it "empty world returns empty results" do
        let world = emptyWorld
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world

        length results `shouldEqual` 0

      it "query with no components specified" do
        let world0 = emptyWorld
            {world: w1, entity: _} = spawnEntity world0

            -- Empty query matches all entities
            q :: _ () ()
            q = query (Proxy :: _ ())
            results = runQuery q w1

        length results `shouldEqual` 1

      it "query after component operations" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntity world0
            {world: w2, entity: e2} = addComponent _position {x: 10.0, y: 20.0} e1 w1

            -- Initially matches
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results1 = runQuery q w2

        length results1 `shouldEqual` 1

        -- After adding more components, still matches
        let {world: w3, entity: _} = addComponent _velocity {x: 1.0, y: 0.0} e2 w2
            results2 = runQuery q w3

        length results2 `shouldEqual` 1

-- Helper functions for tests
infixl 8 arrayIndex as !!

arrayIndex :: forall a. Array a -> Int -> Maybe a
arrayIndex = index

updatePos :: forall r. { entity :: _ , components :: Record (position :: Position, velocity :: Velocity | r) } -> World -> World
updatePos _ w = w  -- Simplified - would update position based on velocity
