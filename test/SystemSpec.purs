module Test.ECS.SystemSpec (systemSpec) where

import Prelude

import Control.Monad.State (state)
import Data.Array (length, filter, foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import ECS.Component (addComponentPure, getComponentPure)

import ECS.Query (query, runQuery)
import ECS.System (System, runSystem, updateComponent, updateComponent_, modifyComponent, modifyComponent_)
import ECS.System as S
import ECS.World (emptyWorld, spawnEntityPure, Entity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

-- Test component types
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }
type Damage = { amount :: Int }
type Mass = Number

systemSpec :: Spec Unit
systemSpec = do
  describe "ECS.System" do
    describe "Basic System Execution" do
      it "runs empty system and returns result" do
        let world = emptyWorld
            emptySystem :: System () () Unit
            emptySystem = pure unit
            { world: _, result } = runSystem emptySystem world
        result `shouldEqual` unit

      it "runs system and returns result" do
        let world = emptyWorld
            countSystem :: System () () Int
            countSystem = pure 42
            { world: _, result } = runSystem countSystem world
        result `shouldEqual` 42

      it "runs system and updates world" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            addPosSystem :: System () (position :: Position) Unit
            addPosSystem = state \w ->
              let { world: w', entity: _ } = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 2.0 } entity w
              in Tuple unit w'
            { world: world', result: _ } = runSystem addPosSystem world1
            -- Use query to find the entity with position
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world'
        length results `shouldEqual` 1

    describe "Query Integration" do
      it "queryInSystem returns results" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            { world: world2, entity: e1' } = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world1
            { world: world3, entity: _ } = addComponentPure (Proxy :: _ "velocity") { x: 0.5, y: 0.5 } e1' world2

            querySystem :: System (position :: Position, velocity :: Velocity) () Int
            querySystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              pure $ length results

            { world: _, result } = runSystem querySystem world3

        result `shouldEqual` 1

      it "queryInSystem with multiple entities" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: e3} = spawnEntityPure world2

            {world: world4, entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world3
            {world: world5, entity: _} = addComponentPure (Proxy :: _ "position") { x: 2.0, y: 2.0 } e2 world4
            {world: world6, entity: _} = addComponentPure (Proxy :: _ "position") { x: 3.0, y: 3.0 } e3 world5

            countSystem :: System (position :: Position) () Int
            countSystem = do
              results <- S.queryFor @(position :: Position)
              pure $ length results

            { world: _, result } = runSystem countSystem world6

        result `shouldEqual` 3

    describe "Component Updates" do
      it "updateComponent modifies entity" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1

            updateSys :: System () (position :: Position) (Entity (position :: Position))
            updateSys = do
              entity'' <- updateComponent (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity'
              pure entity''

            { world: world', result: entity'' } = runSystem updateSys world2
            pos = getComponentPure (Proxy :: _ "position") entity'' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }

      it "multiple updates compose correctly" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 0.5, y: 0.5 } entity2 world2

            updateBoth :: System () (position :: Position, velocity :: Velocity) (Entity (position :: Position, velocity :: Velocity))
            updateBoth = do
              e1 <- updateComponent (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity3
              e2 <- updateComponent (Proxy :: _ "velocity") { x: 5.0, y: 5.0 } e1
              pure e2

            { world: world', result: entity'' } = runSystem updateBoth world3
            pos = getComponentPure (Proxy :: _ "position") entity'' world'
            vel = getComponentPure (Proxy :: _ "velocity") entity'' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }
        vel `shouldEqual` Just { x: 5.0, y: 5.0 }

    describe "System Composition" do
      it "composes two systems sequentially" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world

            system1 :: System () (position :: Position) Int
            system1 = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity w
              in Tuple 42 w'

            system2 :: System () (velocity :: Velocity) String
            system2 = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 0.5, y: 0.5 } entity w
              in Tuple "done" w'

            combined :: System () (position :: Position, velocity :: Velocity)
                               { fst :: Int, snd :: String }
            combined = do
              fst <- system1
              snd <- system2
              pure { fst, snd }

            { world: world', result } = runSystem combined world1
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q world'

        result.fst `shouldEqual` 42
        result.snd `shouldEqual` "done"
        length results `shouldEqual` 1

      it "chains multiple systems" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world

            addPos :: System () (position :: Position) Unit
            addPos = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity w
              in Tuple unit w'

            addVel :: System () (velocity :: Velocity) Unit
            addVel = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 2.0, y: 2.0 } entity w
              in Tuple unit w'

            addHealth :: System () (health :: Health) Unit
            addHealth = state \w ->
              let {world: w', entity: _} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity w
              in Tuple unit w'

            combined = do
              addPos
              addVel
              addHealth

            { world: world', result: _ } = runSystem combined world1
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity, health :: Health))
            results = runQuery q world'

        length results `shouldEqual` 1

    describe "Read-Only Systems" do
      it "read-only system returns correct count" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: _} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world1

            inspectSystem :: System (position :: Position) () Int
            inspectSystem = do
              results <- S.queryFor @(position :: Position)
              pure $ length results

            { world: _, result } = runSystem inspectSystem world2

        result `shouldEqual` 1

      it "read-only system returns analysis" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: _} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } e1 world2
            {world: world4, entity: _} = addComponentPure (Proxy :: _ "health") { current: 0, max: 100 } e2 world3

            countLiving :: System (health :: Health) () Int
            countLiving = do
              results <- S.queryFor @(health :: Health)
              let living = filter (\r -> r.components.health.current > 0) results
              pure $ length living

            { world: _, result } = runSystem countLiving world4

        result `shouldEqual` 1

    describe "Write Systems" do
      it "write system modifies components" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } entity2 world2

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                 (position :: Position)
                                 Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity

            { world: world', result: _ } = runSystem moveSystem world3
            pos = getComponentPure (Proxy :: _ "position") entity3 world'

        pos `shouldEqual` Just { x: 2.0, y: 1.0 }

    describe "mapQuery Helper" do
      it "maps over query results and updates world" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1

            {world: world3, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world2
            {world: world4, entity: e1''} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e1' world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e2 world4
            {world: world6, entity: e2''} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e2' world5

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                 (position :: Position)
                                 Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity

            { world: world', result: _ } = runSystem moveSystem world6
            pos1 = getComponentPure (Proxy :: _ "position") e1'' world'
            pos2 = getComponentPure (Proxy :: _ "position") e2'' world'

        pos1 `shouldEqual` Just { x: 2.0, y: 1.0 }
        pos2 `shouldEqual` Just { x: 5.0, y: 6.0 }

    describe "updateComponent_ (fire-and-forget)" do
      it "updates component without returning entity" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1

            updateSys :: System () (position :: Position) Unit
            updateSys = do
              updateComponent_ (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity'
              -- No need to bind result

            { world: world', result: _ } = runSystem updateSys world2
            pos = getComponentPure (Proxy :: _ "position") entity' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }

      it "works in for_ loops cleanly" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1

            {world: world3, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world2
            {world: world4, entity: e1''} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e1' world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e2 world4
            {world: world6, entity: e2''} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e2' world5

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                 (position :: Position)
                                 Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity
                -- No void needed!

            { world: world', result: _ } = runSystem moveSystem world6
            pos1 = getComponentPure (Proxy :: _ "position") e1'' world'
            pos2 = getComponentPure (Proxy :: _ "position") e2'' world'

        pos1 `shouldEqual` Just { x: 2.0, y: 1.0 }
        pos2 `shouldEqual` Just { x: 5.0, y: 6.0 }

    describe "modifyComponent (read-modify-write)" do
      it "modifies existing component with function" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 10.0 } entity world1

            modifySys :: System () (position :: Position) (Entity (position :: Position))
            modifySys = do
              entity'' <- modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 1.0 }) entity'
              pure entity''

            { world: world', result: entity'' } = runSystem modifySys world2
            pos = getComponentPure (Proxy :: _ "position") entity'' world'

        pos `shouldEqual` Just { x: 6.0, y: 10.0 }

      it "transforms component values" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity world1

            damageSys :: System () (health :: Health) (Entity (health :: Health))
            damageSys = do
              entity'' <- modifyComponent (Proxy :: _ "health") (\h -> h { current = h.current - 25 }) entity'
              pure entity''

            { world: world', result: entity'' } = runSystem damageSys world2
            health = getComponentPure (Proxy :: _ "health") entity'' world'

        health `shouldEqual` Just { current: 75, max: 100 }

      it "chains multiple modifications" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 0.0, y: 0.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 2.0 } entity2 world2

            modifyBoth :: System () (position :: Position, velocity :: Velocity) Unit
            modifyBoth = do
              e1 <- modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 10.0 }) entity3
              void $ modifyComponent (Proxy :: _ "velocity") (\v -> v { y = v.y * 2.0 }) e1

            { world: world', result: _ } = runSystem modifyBoth world3
            pos = getComponentPure (Proxy :: _ "position") entity3 world'
            vel = getComponentPure (Proxy :: _ "velocity") entity3 world'

        pos `shouldEqual` Just { x: 10.0, y: 0.0 }
        vel `shouldEqual` Just { x: 1.0, y: 4.0 }

    describe "modifyComponent_ (fire-and-forget modify)" do
      it "modifies component without returning entity" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity world1

            damageSys :: System () (health :: Health) Unit
            damageSys = do
              modifyComponent_ (Proxy :: _ "health") (\h -> h { current = h.current - 50 }) entity'
              -- No need to bind result

            { world: world', result: _ } = runSystem damageSys world2
            health = getComponentPure (Proxy :: _ "health") entity' world'

        health `shouldEqual` Just { current: 50, max: 100 }

      it "works in for_ loops for batch modifications" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: e3} = spawnEntityPure world2

            {world: world4, entity: e1'} = addComponentPure (Proxy :: _ "health") { current: 50, max: 100 } e1 world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "health") { current: 30, max: 100 } e2 world4
            {world: world6, entity: e3'} = addComponentPure (Proxy :: _ "health") { current: 80, max: 100 } e3 world5

            healSystem :: System (health :: Health) (health :: Health) Unit
            healSystem = do
              results <- S.queryFor @(health :: Health)
              for_ results \r -> do
                modifyComponent_ (Proxy :: _ "health") (\h -> h { current = h.current + 10 }) r.entity

            { world: world', result: _ } = runSystem healSystem world6
            h1 = getComponentPure (Proxy :: _ "health") e1' world'
            h2 = getComponentPure (Proxy :: _ "health") e2' world'
            h3 = getComponentPure (Proxy :: _ "health") e3' world'

        h1 `shouldEqual` Just { current: 60, max: 100 }
        h2 `shouldEqual` Just { current: 40, max: 100 }
        h3 `shouldEqual` Just { current: 90, max: 100 }

    describe "foldQuery Helper" do
      it "folds over query results" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: e3} = spawnEntityPure world2

            {world: world4, entity: _} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } e1 world3
            {world: world5, entity: _} = addComponentPure (Proxy :: _ "health") { current: 50, max: 100 } e2 world4
            {world: world6, entity: _} = addComponentPure (Proxy :: _ "health") { current: 0, max: 100 } e3 world5

            sumHealth :: System (health :: Health) () Int
            sumHealth = do
              results <- S.queryFor @(health :: Health)
              pure $ foldl (\acc r -> acc + r.components.health.current) 0 results

            { world: _, result } = runSystem sumHealth world6

        result `shouldEqual` 150

    describe "filterQuery Helper" do
      it "filters and processes matching results" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: e3} = spawnEntityPure world2

            {world: world4, entity: e1'} = addComponentPure (Proxy :: _ "health") { current: 30, max: 100 } e1 world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "health") { current: 80, max: 100 } e2 world4
            {world: world6, entity: e3'} = addComponentPure (Proxy :: _ "health") { current: 10, max: 100 } e3 world5

            healLowHealth :: System (health :: Health) (health :: Health) Unit
            healLowHealth = do
              results <- S.queryFor @(health :: Health)
              let filtered = filter (\r -> r.components.health.current < 50) results
              for_ filtered \r -> do
                updateComponent_ (Proxy :: _ "health") { current: 100, max: 100 } r.entity

            { world: world', result: _ } = runSystem healLowHealth world6
            h1 = getComponentPure (Proxy :: _ "health") e1' world'
            h2 = getComponentPure (Proxy :: _ "health") e2' world'
            h3 = getComponentPure (Proxy :: _ "health") e3' world'

        h1 `shouldEqual` Just { current: 100, max: 100 }
        h2 `shouldEqual` Just { current: 80, max: 100 }
        h3 `shouldEqual` Just { current: 100, max: 100 }

    describe "Integration Tests" do
      it "complete workflow: spawn, add, query, update" do
        let world = emptyWorld

            setupSystem :: System () (position :: Position, velocity :: Velocity) Unit
            setupSystem = state \w -> do
              let {world: w1, entity: e1} = spawnEntityPure w
                  {world: w2, entity: e2} = spawnEntityPure w1
                  {world: w3, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 0.0, y: 0.0 } e1 w2
                  {world: w4, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 1.0 } e1' w3
                  {world: w5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e2 w4
                  {world: w6, entity: _} = addComponentPure (Proxy :: _ "velocity") { x: -1.0, y: -1.0 } e2' w5
              Tuple unit w6

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                 (position :: Position)
                                 Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity

            combined = do
              setupSystem
              moveSystem

            { world: world', result: _ } = runSystem combined world

            q = query (Proxy :: _ (position :: Position))
            results = runQuery q world'

        length results `shouldEqual` 2

    describe "State Monad Systems (New API)" do
      it "runs State-based system and returns result" do
        let world = emptyWorld
            countSystem :: System () () Int
            countSystem = pure 42
            { world: _, result } = runSystem countSystem world
        result `shouldEqual` 42

      it "query returns results in State monad" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            { world: world2, entity: _ } = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world1

            querySystem :: System (position :: Position) () Int
            querySystem = do
              results <- S.queryFor @(position :: Position)
              pure $ length results

            { world: _, result } = runSystem querySystem world2

        result `shouldEqual` 1

      it "updateComponent modifies entity in State monad" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1

            updateSys :: System () (position :: Position) (Entity (position :: Position))
            updateSys = do
              entity'' <- updateComponent (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity'
              pure entity''

            { world: world', result: entity'' } = runSystem updateSys world2
            pos = getComponentPure (Proxy :: _ "position") entity'' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }

      it "multiple updates with do-notation (no nested runSystem!)" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 0.5, y: 0.5 } entity2 world2

            updateBoth :: System () (position :: Position, velocity :: Velocity) (Entity (position :: Position, velocity :: Velocity))
            updateBoth = do
              e1 <- updateComponent (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity3
              e2 <- updateComponent (Proxy :: _ "velocity") { x: 5.0, y: 5.0 } e1
              pure e2

            { world: world', result: entity'' } = runSystem updateBoth world3
            pos = getComponentPure (Proxy :: _ "position") entity'' world'
            vel = getComponentPure (Proxy :: _ "velocity") entity'' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }
        vel `shouldEqual` Just { x: 5.0, y: 5.0 }

      it "State monad with for_ for clean iteration" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1

            {world: world3, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world2
            {world: world4, entity: e1''} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e1' world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e2 world4
            {world: world6, entity: e2''} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e2' world5

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                  (position :: Position)
                                  Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity

            { world: world', result: _ } = runSystem moveSystem world6
            pos1 = getComponentPure (Proxy :: _ "position") e1'' world'
            pos2 = getComponentPure (Proxy :: _ "position") e2'' world'

        pos1 `shouldEqual` Just { x: 2.0, y: 1.0 }
        pos2 `shouldEqual` Just { x: 5.0, y: 6.0 }

    describe "updateComponent_ (fire-and-forget)" do
      it "updates component without returning entity" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } entity world1

            updateSys :: System () (position :: Position) Unit
            updateSys = do
              updateComponent_ (Proxy :: _ "position") { x: 10.0, y: 10.0 } entity'
              -- No need to bind result

            { world: world', result: _ } = runSystem updateSys world2
            pos = getComponentPure (Proxy :: _ "position") entity' world'

        pos `shouldEqual` Just { x: 10.0, y: 10.0 }

      it "works in for_ loops cleanly" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1

            {world: world3, entity: e1'} = addComponentPure (Proxy :: _ "position") { x: 1.0, y: 1.0 } e1 world2
            {world: world4, entity: e1''} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 0.0 } e1' world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 5.0 } e2 world4
            {world: world6, entity: e2''} = addComponentPure (Proxy :: _ "velocity") { x: 0.0, y: 1.0 } e2' world5

            moveSystem :: System (position :: Position, velocity :: Velocity)
                                 (position :: Position)
                                 Unit
            moveSystem = do
              results <- S.queryFor @(position :: Position, velocity :: Velocity)
              for_ results \r -> do
                let newPos = { x: r.components.position.x + r.components.velocity.x
                             , y: r.components.position.y + r.components.velocity.y
                             }
                updateComponent_ (Proxy :: _ "position") newPos r.entity
                -- No void needed!

            { world: world', result: _ } = runSystem moveSystem world6
            pos1 = getComponentPure (Proxy :: _ "position") e1'' world'
            pos2 = getComponentPure (Proxy :: _ "position") e2'' world'

        pos1 `shouldEqual` Just { x: 2.0, y: 1.0 }
        pos2 `shouldEqual` Just { x: 5.0, y: 6.0 }

    describe "modifyComponent (read-modify-write)" do
      it "modifies existing component with function" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "position") { x: 5.0, y: 10.0 } entity world1

            modifySys :: System () (position :: Position) (Entity (position :: Position))
            modifySys = do
              entity'' <- modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 1.0 }) entity'
              pure entity''

            { world: world', result: entity'' } = runSystem modifySys world2
            pos = getComponentPure (Proxy :: _ "position") entity'' world'

        pos `shouldEqual` Just { x: 6.0, y: 10.0 }

      it "transforms component values" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity world1

            damageSys :: System () (health :: Health) (Entity (health :: Health))
            damageSys = do
              entity'' <- modifyComponent (Proxy :: _ "health") (\h -> h { current = h.current - 25 }) entity'
              pure entity''

            { world: world', result: entity'' } = runSystem damageSys world2
            health = getComponentPure (Proxy :: _ "health") entity'' world'

        health `shouldEqual` Just { current: 75, max: 100 }

      it "chains multiple modifications" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity2} = addComponentPure (Proxy :: _ "position") { x: 0.0, y: 0.0 } entity world1
            {world: world3, entity: entity3} = addComponentPure (Proxy :: _ "velocity") { x: 1.0, y: 2.0 } entity2 world2

            modifyBoth :: System () (position :: Position, velocity :: Velocity) Unit
            modifyBoth = do
              e1 <- modifyComponent (Proxy :: _ "position") (\p -> p { x = p.x + 10.0 }) entity3
              void $ modifyComponent (Proxy :: _ "velocity") (\v -> v { y = v.y * 2.0 }) e1

            { world: world', result: _ } = runSystem modifyBoth world3
            pos = getComponentPure (Proxy :: _ "position") entity3 world'
            vel = getComponentPure (Proxy :: _ "velocity") entity3 world'

        pos `shouldEqual` Just { x: 10.0, y: 0.0 }
        vel `shouldEqual` Just { x: 1.0, y: 4.0 }

    describe "modifyComponent_ (fire-and-forget modify)" do
      it "modifies component without returning entity" do
        let world = emptyWorld
            {world: world1, entity: entity} = spawnEntityPure world
            {world: world2, entity: entity'} = addComponentPure (Proxy :: _ "health") { current: 100, max: 100 } entity world1

            damageSys :: System () (health :: Health) Unit
            damageSys = do
              modifyComponent_ (Proxy :: _ "health") (\h -> h { current = h.current - 50 }) entity'
              -- No need to bind result

            { world: world', result: _ } = runSystem damageSys world2
            health = getComponentPure (Proxy :: _ "health") entity' world'

        health `shouldEqual` Just { current: 50, max: 100 }

      it "works in for_ loops for batch modifications" do
        let world = emptyWorld
            {world: world1, entity: e1} = spawnEntityPure world
            {world: world2, entity: e2} = spawnEntityPure world1
            {world: world3, entity: e3} = spawnEntityPure world2

            {world: world4, entity: e1'} = addComponentPure (Proxy :: _ "health") { current: 50, max: 100 } e1 world3
            {world: world5, entity: e2'} = addComponentPure (Proxy :: _ "health") { current: 30, max: 100 } e2 world4
            {world: world6, entity: e3'} = addComponentPure (Proxy :: _ "health") { current: 80, max: 100 } e3 world5

            healSystem :: System (health :: Health) (health :: Health) Unit
            healSystem = do
              results <- S.queryFor @(health :: Health)
              for_ results \r -> do
                modifyComponent_ (Proxy :: _ "health") (\h -> h { current = h.current + 10 }) r.entity

            { world: world', result: _ } = runSystem healSystem world6
            h1 = getComponentPure (Proxy :: _ "health") e1' world'
            h2 = getComponentPure (Proxy :: _ "health") e2' world'
            h3 = getComponentPure (Proxy :: _ "health") e3' world'

        h1 `shouldEqual` Just { current: 60, max: 100 }
        h2 `shouldEqual` Just { current: 40, max: 100 }
        h3 `shouldEqual` Just { current: 90, max: 100 }
