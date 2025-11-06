module Test.ECS.WorldSpec where

import Prelude

import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..))

import ECS.Entity (entityIndex, entityVersion)
import ECS.World (World, Entity, emptyWorld, despawnEntityPure, hasEntity, spawnEntityPure, unEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

worldSpec :: Spec Unit
worldSpec = do
  describe "ECS.World" do

    -- World Creation Tests
    describe "World Creation" do

      it "emptyWorld has no entities" do
        let world = emptyWorld
        world.entities.nextIndex `shouldEqual` 0

      it "emptyWorld has no archetypes" do
        let world = emptyWorld
        Map.isEmpty world.archetypes `shouldEqual` true

      it "emptyWorld EntityManager is empty" do
        let world = emptyWorld
        world.entities.nextIndex `shouldEqual` 0
        Map.isEmpty world.entityLocations `shouldEqual` true

    -- Entity Spawn Tests
    describe "Entity Spawn" do

      it "spawnEntityPure creates valid entity" do
        let result = spawnEntityPure emptyWorld
        hasEntity result.entity result.world `shouldEqual` true

      it "first entity has index 0, version 0" do
        let result = spawnEntityPure emptyWorld
            entityId = unEntity result.entity
        entityIndex entityId `shouldEqual` 0
        entityVersion entityId `shouldEqual` 0

      it "spawn multiple entities increments indices" do
        let r1 = spawnEntityPure emptyWorld
            r2 = spawnEntityPure r1.world
            r3 = spawnEntityPure r2.world
            ids = map (entityIndex <<< unEntity) [r1.entity, r2.entity, r3.entity]
        ids `shouldEqual` [0, 1, 2]

      it "spawned entity stored in empty archetype" do
        let result = spawnEntityPure emptyWorld
            entityId = unEntity result.entity
            maybeArchId = Map.lookup (entityIndex entityId) result.world.entityLocations
        maybeArchId `shouldEqual` Just ""

      it "entityLocations updated correctly" do
        let r1 = spawnEntityPure emptyWorld
            r2 = spawnEntityPure r1.world
            idx1 = entityIndex (unEntity r1.entity)
            idx2 = entityIndex (unEntity r2.entity)
            loc1 = Map.lookup idx1 r2.world.entityLocations
            loc2 = Map.lookup idx2 r2.world.entityLocations
        loc1 `shouldEqual` Just ""
        loc2 `shouldEqual` Just ""

    -- Entity Despawn Tests
    describe "Entity Despawn" do

      it "despawn removes entity from world" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
        hasEntity r.entity world2 `shouldEqual` false

      it "despawn updates EntityManager (version increments)" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
            -- Create new entity, should reuse index with incremented version
            r2 = spawnEntityPure world2
            id1 = unEntity r.entity
            id2 = unEntity r2.entity
        entityIndex id1 `shouldEqual` entityIndex id2
        entityVersion id2 `shouldEqual` (entityVersion id1 + 1)

      it "despawned entity fails hasEntity check" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
        hasEntity r.entity world2 `shouldEqual` false

      it "despawn removes from entityLocations" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
            idx = entityIndex (unEntity r.entity)
        Map.lookup idx world2.entityLocations `shouldEqual` Nothing

      it "despawn from archetype succeeds" do
        let r1 = spawnEntityPure emptyWorld
            r2 = spawnEntityPure r1.world
            world3 = despawnEntityPure r1.entity r2.world
        -- r2 should still exist, r1 should not
        hasEntity r1.entity world3 `shouldEqual` false
        hasEntity r2.entity world3 `shouldEqual` true

    -- Entity Validation Tests
    describe "Entity Validation" do

      it "hasEntity returns true for valid entity" do
        let r = spawnEntityPure emptyWorld
        hasEntity r.entity r.world `shouldEqual` true

      it "hasEntity returns false for despawned entity" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
        hasEntity r.entity world2 `shouldEqual` false

      it "hasEntity validates version correctly" do
        let r1 = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r1.entity r1.world
            r2 = spawnEntityPure world2  -- Reuses index with new version
        -- Old entity reference should be invalid
        hasEntity r1.entity r2.world `shouldEqual` false
        -- New entity reference should be valid
        hasEntity r2.entity r2.world `shouldEqual` true

    -- Archetype Tests
    describe "Archetype Management" do

      it "empty archetype created automatically" do
        let result = spawnEntityPure emptyWorld
            idx = entityIndex (unEntity result.entity)
        -- Empty archetype should exist in world (checked by entityLocations)
        Map.lookup idx result.world.entityLocations `shouldEqual` Just ""

      it "multiple entities in same archetype" do
        let r1 = spawnEntityPure emptyWorld
            r2 = spawnEntityPure r1.world
            r3 = spawnEntityPure r2.world
            idx1 = entityIndex (unEntity r1.entity)
            idx2 = entityIndex (unEntity r2.entity)
            idx3 = entityIndex (unEntity r3.entity)
            loc1 = Map.lookup idx1 r3.world.entityLocations
            loc2 = Map.lookup idx2 r3.world.entityLocations
            loc3 = Map.lookup idx3 r3.world.entityLocations
        -- All should be in empty archetype
        loc1 `shouldEqual` Just ""
        loc2 `shouldEqual` Just ""
        loc3 `shouldEqual` Just ""

      it "entityLocations tracks correct archetypes" do
        let r1 = spawnEntityPure emptyWorld
            r2 = spawnEntityPure r1.world
            idx1 = entityIndex (unEntity r1.entity)
            idx2 = entityIndex (unEntity r2.entity)
        Map.lookup idx1 r2.world.entityLocations `shouldEqual` Just ""
        Map.lookup idx2 r2.world.entityLocations `shouldEqual` Just ""

    -- Lifecycle Tests
    describe "Lifecycle Tests" do

      it "spawn then despawn leaves clean state" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
        Map.isEmpty world2.entityLocations `shouldEqual` true

      it "spawn, despawn, spawn reuses index" do
        let r1 = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r1.entity r1.world
            r2 = spawnEntityPure world2
            idx1 = entityIndex (unEntity r1.entity)
            idx2 = entityIndex (unEntity r2.entity)
        idx1 `shouldEqual` idx2

      it "multiple spawn/despawn cycles work" do
        let r1 = spawnEntityPure emptyWorld
            w2 = despawnEntityPure r1.entity r1.world
            r2 = spawnEntityPure w2
            w3 = despawnEntityPure r2.entity r2.world
            r3 = spawnEntityPure w3
            idx = entityIndex (unEntity r3.entity)
        idx `shouldEqual` 0  -- Should reuse index 0

      it "100 entities spawn successfully" do
        let spawnN :: Int -> World -> { world :: World, entities :: Array (Entity ()) }
            spawnN 0 world = { world, entities: [] }
            spawnN n world =
              let r = spawnEntityPure world
                  rest = spawnN (n - 1) r.world
              in { world: rest.world, entities: [r.entity] <> rest.entities }

            result = spawnN 100 emptyWorld
        -- All should be valid
        all (\e -> hasEntity e result.world) result.entities `shouldEqual` true

    -- Edge Cases
    describe "Edge Cases" do

      it "despawn non-existent entity does nothing" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
            -- Despawn again (already gone)
            world3 = despawnEntityPure r.entity world2
        Map.isEmpty world3.entityLocations `shouldEqual` true

      it "despawn already-despawned entity is idempotent" do
        let r = spawnEntityPure emptyWorld
            world2 = despawnEntityPure r.entity r.world
            world3 = despawnEntityPure r.entity world2
        -- Should not crash, state should be same
        hasEntity r.entity world2 `shouldEqual` false
        hasEntity r.entity world3 `shouldEqual` false

      it "can create 1000 entities (stress test)" do
        let spawnN :: Int -> World -> World
            spawnN 0 world = world
            spawnN n world = spawnN (n - 1) (spawnEntityPure world).world

            finalWorld = spawnN 1000 emptyWorld
        finalWorld.entities.nextIndex `shouldEqual` 1000

      it "empty world operations don't crash" do
        let world = emptyWorld
            r = spawnEntityPure world
            world2 = despawnEntityPure r.entity r.world
        Map.isEmpty world2.entityLocations `shouldEqual` true

-- Helper to check if all elements satisfy predicate
all :: forall a. (a -> Boolean) -> Array a -> Boolean
all f arr = foldr (\x acc -> f x && acc) true arr
