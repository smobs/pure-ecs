-- | ECS Integration Tests
-- |
-- | These tests verify that all ECS phases work together correctly in
-- | realistic scenarios. They cover end-to-end workflows, performance
-- | characteristics, and regression testing.
module Test.ECS.IntegrationSpec (integrationSpec) where

import Prelude

import Control.Monad.State (state)
import Data.Array (foldl, length, range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ECS.Component (addComponentPure, getComponentPure, hasComponent, removeComponentPure)
import ECS.Entity (entityIndex)
import ECS.Query (query, runQuery, without)
import ECS.System (System, runSystem, updateComponent, updateComponent_)
import ECS.World (emptyWorld, spawnEntityPure, despawnEntityPure, unEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

-- Test component types
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }
type Damage = { amount :: Int }

integrationSpec :: Spec Unit
integrationSpec = do
  describe "ECS Integration" do

    describe "Full Workflow: Spawn → Add → Query → Update → Despawn" do
      it "completes full entity lifecycle" do
        let world = emptyWorld

            -- Spawn entity
            {world: world1, entity: entity} = spawnEntityPure world

            -- Add components
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 0.5, y: 0.5 } entity2 world2
            {world: world4, entity: entity4} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity3 world3

            -- Query to verify
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity, health :: Health))
            results = runQuery q world4

            -- Update component via system
            updateSys :: System (position :: Position) (position :: Position) Unit
            updateSys = do
              updateComponent_ (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity4

            {world: world5, result: _} = runSystem updateSys world4

            -- Verify update
            pos = getComponentPure (Proxy :: _ "position") entity4 world5

            -- Despawn
            world6 = despawnEntityPure entity world5

            -- Verify despawn
            q2 = query (Proxy :: _ (position :: Position))
            results2 = runQuery q2 world6

        length results `shouldEqual` 1
        pos `shouldEqual` Just { x: 10.0, y: 10.0 }
        length results2 `shouldEqual` 0

      it "handles multiple entities through full lifecycle" do
        let world = emptyWorld

            -- Spawn 3 entities
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            {world: w3, entity: e3} = spawnEntityPure w2

            -- Add components to all
            {world: w4, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 w3
            {world: w5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 2.0, y: 2.0 } e2 w4
            {world: w6, entity: _} = addComponentPure (Proxy :: _ "position") { x: 3.0, y: 3.0 } e3 w5

            -- Add velocity to 2 of them
            {world: w7, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e1' w6
            {world: w8, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e2' w7

            -- Query moving entities
            qMove = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            moving = runQuery qMove w8

            -- Query stationary entities
            qStill = query (Proxy :: _ (position :: Position)) # without (Proxy :: _ "velocity")
            stationary = runQuery qStill w8

            -- Despawn one moving entity
            w9 = despawnEntityPure e1 w8

            movingAfter = runQuery qMove w9

        length moving `shouldEqual` 2
        length stationary `shouldEqual` 1
        length movingAfter `shouldEqual` 1

    describe "Multi-System Pipelines" do
      it "chains 3 systems with different access patterns" do
        let world = emptyWorld
            {world: w1, entity: e} = spawnEntityPure world

            -- System 1: Add position
            sys1 :: System () (position :: Position) Unit
            sys1 = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "position") { x: 0.0, y: 0.0 } e w
              in Tuple unit w'

            -- System 2: Add velocity
            sys2 :: System () (velocity :: Velocity) Unit
            sys2 = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 1.0 } e w
              in Tuple unit w'

            -- System 3: Add health
            sys3 :: System () (health :: Health) Unit
            sys3 = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } e w
              in Tuple unit w'

            -- Compose all three with do-notation
            combined123 = do
              sys1
              sys2
              sys3

            {world: w2, result: _} = runSystem combined123 w1

            -- Verify all components added
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity, health :: Health))
            results = runQuery q w2

        length results `shouldEqual` 1

      it "runs read and write systems in sequence" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e1'} = addComponentPure (Proxy :: _ "health") { current: 50, max: 100 } e1 w1
            {world: w3, entity: e2} = spawnEntityPure w2
            {world: w4, entity: _} = addComponentPure (Proxy :: _ "health") { current: 75, max: 100 } e2 w3

            -- Read system: count entities
            countSys :: System (health :: Health) () Int
            countSys = state \w ->
              let q = query (Proxy :: _ (health :: Health))
                  results = runQuery q w
              in Tuple (length results) w

            -- Write system: heal all
            healSys :: System (health :: Health) (health :: Health) Unit
            healSys = state \w ->
              let q = query (Proxy :: _ (health :: Health))
                  results = runQuery q w
                  heal r world =
                    let {world: w', result: _} = runSystem (updateComponent (Proxy :: _ "health") { current: 100, max: 100 } r.entity) world
                    in w'
                  w' = foldl (\acc r -> heal r acc) w results
              in Tuple unit w'

            {world: w5, result: count1} = runSystem countSys w4
            {world: w6, result: _} = runSystem healSys w5
            {world: w7, result: count2} = runSystem countSys w6

            -- Check healed values
            h1 = getComponentPure (Proxy :: _ "health") e1' w7

        count1 `shouldEqual` 2
        count2 `shouldEqual` 2
        h1 `shouldEqual` Just { current: 100, max: 100 }

    describe "Performance Tests" do
      it "handles 100 entities efficiently" do
        let world = emptyWorld

            -- Helper to spawn and setup entity
            spawnWithPos i w =
              let {world: w1, entity: e} = spawnEntityPure w
                  {world: w2, entity: _} = addComponentPure (Proxy :: _ "position") { x: toNumber i, y: 0.0 } e w1
              in w2

            -- Spawn 100 entities
            world' = foldl (\w i -> spawnWithPos i w) world (range 0 99)

            -- Query all
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world'

        length results `shouldEqual` 100

      it "handles 500 entities with multiple components" do
        let world = emptyWorld

            spawnComplex i w =
              let {world: w1, entity: e} = spawnEntityPure w
                  {world: w2, entity: e'} = addComponentPure (Proxy :: _ "position") { x: toNumber i, y: toNumber i } e w1
                  {world: w3, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 1.0 } e' w2
              in w3

            world' = foldl (\w i -> spawnComplex i w) world (range 0 499)

            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q world'

        length results `shouldEqual` 500

      it "system execution scales linearly" do
        let world = emptyWorld

            setupN n w =
              if n <= 0 then w
              else
                let {world: w1, entity: e} = spawnEntityPure w
                    {world: w2, entity: e'} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } e w1
                    {world: w3, entity: _} = addComponentPure (Proxy :: _ "damage") { amount: 10 } e' w2
                in setupN (n - 1) w3

            world' = setupN 200 world

            damageSys :: System (health :: Health, damage :: Damage) (health :: Health) Int
            damageSys = state \w ->
              let q = query (Proxy :: _ (health :: Health, damage :: Damage))
                  results = runQuery q w
                  apply r world =
                    let newHealth = r.components.health.current - r.components.damage.amount
                        h' = r.components.health { current = newHealth }
                        {world: w', result: _} = runSystem (updateComponent (Proxy :: _ "health") h' r.entity) world
                    in w'
                  w' = foldl (\acc r -> apply r acc) w results
              in Tuple (length results) w'

            {world: _, result: count} = runSystem damageSys world'

        count `shouldEqual` 200

    describe "Regression Tests" do
      it "entity recycling works with all operations" do
        let world = emptyWorld

            -- Spawn and despawn to populate free list
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            w3 = despawnEntityPure e1 w2
            w4 = despawnEntityPure e2 w3

            -- Spawn new entity (should recycle)
            {world: w5, entity: e3} = spawnEntityPure w4
            {world: w6, entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e3 w5

            -- Query should find it
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w6

        length results `shouldEqual` 1
        entityIndex (unEntity e3) `shouldEqual` 1  -- Recycled entity 2's index

      it "recycled entity does not inherit old entity's component data" do
        let world = emptyWorld

            -- Step 1: Create entity with specific component values
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e1'} = addComponentPure (Proxy :: _ "position")
              { x: 100.0, y: 200.0 } e1 w1
            {world: w3, entity: _} = addComponentPure (Proxy :: _ "velocity")
              { x: 5.0, y: 10.0 } e1' w2

            -- Step 2: Despawn the entity (index goes to free list)
            w4 = despawnEntityPure e1 w3

            -- Step 3: Spawn new entity (should recycle e1's index)
            {world: w5, entity: e2} = spawnEntityPure w4

            -- Step 4: Add components with DIFFERENT values to new entity
            {world: w6, entity: e2'} = addComponentPure (Proxy :: _ "position")
              { x: 1.0, y: 2.0 } e2 w5
            {world: w7, entity: e2''} = addComponentPure (Proxy :: _ "velocity")
              { x: 0.5, y: 0.25 } e2' w6

            -- Verify: New entity should have NEW values, not old ones
            pos = getComponentPure (Proxy :: _ "position") e2'' w7
            vel = getComponentPure (Proxy :: _ "velocity") e2'' w7

        -- These assertions should pass - new entity must have its own data
        pos `shouldEqual` Just { x: 1.0, y: 2.0 }  -- NOT { x: 100.0, y: 200.0 }!
        vel `shouldEqual` Just { x: 0.5, y: 0.25 }  -- NOT { x: 5.0, y: 10.0 }!

      it "recycled entity with single component does not inherit old data" do
        let world = emptyWorld

            -- Simpler test: just position component
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: _} = addComponentPure (Proxy :: _ "position")
              { x: 100.0, y: 200.0 } e1 w1

            -- Despawn
            w3 = despawnEntityPure e1 w2

            -- Spawn new entity (recycles index)
            {world: w4, entity: e2} = spawnEntityPure w3
            {world: w5, entity: e2'} = addComponentPure (Proxy :: _ "position")
              { x: 1.0, y: 2.0 } e2 w4

            -- Verify
            pos = getComponentPure (Proxy :: _ "position") e2' w5

        pos `shouldEqual` Just { x: 1.0, y: 2.0 }  -- NOT { x: 100.0, y: 200.0 }!

      it "component migration preserves data across phases" do
        let world = emptyWorld
            {world: w1, entity: e} = spawnEntityPure world

            -- Phase 1: Add position
            {world: w2, entity: e2} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 10.0 } e w1

            -- Phase 2: Add velocity (triggers archetype migration)
            {world: w3, entity: e3} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 2.0 } e2 w2

            -- Phase 3: Add health (another migration)
            {world: w4, entity: e4} = addComponentPure (Proxy :: _ "health") { current: 50, max: 100 } e3 w3

            -- Phase 4: Remove velocity (migration back)
            {world: w5, entity: e5} = removeComponentPure (Proxy :: _ "velocity") e4 w4

            -- Verify all data preserved and velocity removed
            pos = getComponentPure (Proxy :: _ "position") e5 w5
            health = getComponentPure (Proxy :: _ "health") e5 w5
            -- Query to verify velocity is gone
            qVel = query (Proxy :: _ (velocity :: Velocity))
            velResults = runQuery qVel w5

        pos `shouldEqual` Just { x: 5.0, y: 10.0 }
        health `shouldEqual` Just { current: 50, max: 100 }
        length velResults `shouldEqual` 0  -- No entities with velocity

      it "queries work after extensive world modifications" do
        let world = emptyWorld

            -- Setup: 5 entities with various components
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: e4} = spawnEntityPure w3
            {world: w5, entity: e5} = spawnEntityPure w4

            {world: w6, entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 w5
            {world: w7, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 2.0, y: 2.0 } e2 w6
            {world: w8, entity: e2''} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e2' w7
            {world: w9, entity: _} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } e3 w8
            {world: w10, entity: e4'} = addComponentPure (Proxy :: _ "position") { x: 4.0, y: 4.0 } e4 w9
            {world: w11, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e4' w10
            {world: w12, entity: _} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e5 w11

            -- Modifications: despawn some, modify others
            w13 = despawnEntityPure e3 w12
            {world: w14, entity: _} = removeComponentPure (Proxy :: _ "velocity") e2'' w13

            -- Queries after modifications
            qPos = query (Proxy :: _ (position :: Position))
            qVel = query (Proxy :: _ (velocity :: Velocity))
            qMoving = query (Proxy :: _ (position :: Position, velocity :: Velocity))

            posResults = runQuery qPos w14
            velResults = runQuery qVel w14
            movingResults = runQuery qMoving w14

        length posResults `shouldEqual` 4  -- e1, e2, e4, e5
        length velResults `shouldEqual` 1  -- e4 only
        length movingResults `shouldEqual` 1  -- e4 only

    describe "Edge Cases" do
      it "empty queries return empty results" do
        let world = emptyWorld
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world

        length results `shouldEqual` 0

      it "system on empty world succeeds" do
        let world = emptyWorld
            sys :: System (position :: Position) () Int
            sys = state \w ->
              let q = query (Proxy :: _ (position :: Position))
                  results = runQuery q w
              in Tuple (length results) w

            {world: _, result} = runSystem sys world

        result `shouldEqual` 0

      it "despawning all entities leaves clean state" do
        let world = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world
            {world: w2, entity: e2} = spawnEntityPure w1
            {world: w3, entity: e3} = spawnEntityPure w2

            w4 = despawnEntityPure e1 w3
            w5 = despawnEntityPure e2 w4
            w6 = despawnEntityPure e3 w5

            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w6

        length results `shouldEqual` 0
