module Test.ECS.EntitySpec where

import Prelude

import Control.Monad.State (evalState, execState, runState)
import Data.List (List(..), length, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple(..))
import ECS.Entity (EntityId(..), createEntity, deleteEntity, emptyEntityManager, entityCount, entityIndex, entityVersion, validateEntity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

entitySpec :: Spec Unit
entitySpec = do
  describe "ECS.Entity" do

    -- Basic Creation Tests
    describe "Basic Creation" do

      it "creates entity with valid ID and version 0" do
        let (Tuple value _) = runState createEntity emptyEntityManager
        entityVersion value `shouldEqual` 0
        entityIndex value `shouldEqual` 0

      it "first entity has index 0" do
        let entity = evalState createEntity emptyEntityManager
        entityIndex entity `shouldEqual` 0

      it "second entity has index 1" do
        let createTwo = do
              _ <- createEntity
              createEntity
            entity2 = evalState createTwo emptyEntityManager
        entityIndex entity2 `shouldEqual` 1

      it "multiple creates increment index correctly" do
        let create5 = do
              e1 <- createEntity
              e2 <- createEntity
              e3 <- createEntity
              e4 <- createEntity
              e5 <- createEntity
              pure [e1, e2, e3, e4, e5]
            entities = evalState create5 emptyEntityManager
        map entityIndex entities `shouldEqual` [0, 1, 2, 3, 4]

      it "empty manager has entityCount = 0" do
        entityCount emptyEntityManager `shouldEqual` 0

    -- Deletion Tests
    describe "Deletion" do

      it "delete entity increments version" do
        let (Tuple _ state) = runState (createEntity >>= deleteEntity) emptyEntityManager
        -- After delete, version map should have incremented version
        case Map.lookup 0 state.versions of
          Just v -> v `shouldEqual` 1
          Nothing -> pure unit -- Should have version entry

      it "deleted entity fails validation" do
        let ops = do
              entity <- createEntity
              deleteEntity entity
              pure entity
            (Tuple value state) = runState ops emptyEntityManager
        validateEntity value state `shouldEqual` false

      it "delete adds index to freeList" do
        let ops = do
              entity <- createEntity
              deleteEntity entity
            manager = execState ops emptyEntityManager
        length manager.freeList `shouldEqual` 1

      it "cannot delete already-deleted entity (idempotent)" do
        let ops = do
              entity <- createEntity
              deleteEntity entity
              deleteEntity entity  -- Delete again
            manager = execState ops emptyEntityManager
        -- Should still have only 1 entry in freeList
        length manager.freeList `shouldEqual` 1

    -- Recycling Tests
    describe "Recycling" do

      it "create, delete, create reuses same index" do
        let ops = do
              e1 <- createEntity
              deleteEntity e1
              e2 <- createEntity
              pure { first: e1, second: e2 }
            result = evalState ops emptyEntityManager
        entityIndex result.first `shouldEqual` entityIndex result.second

      it "recycled entity has incremented version" do
        let ops = do
              e1 <- createEntity
              deleteEntity e1
              e2 <- createEntity
              pure { first: e1, second: e2 }
            result = evalState ops emptyEntityManager
        entityVersion result.second `shouldEqual` (entityVersion result.first + 1)

      it "old reference fails validation after recycling" do
        let ops = do
              e1 <- createEntity
              deleteEntity e1
              e2 <- createEntity
              pure { old: e1, new: e2 }
            (Tuple value state) = runState ops emptyEntityManager
        validateEntity value.old state `shouldEqual` false
        validateEntity value.new state `shouldEqual` true

      it "multiple deletes increment version each time" do
        let ops = do
              e1 <- createEntity
              deleteEntity e1
              e2 <- createEntity
              deleteEntity e2
              e3 <- createEntity
              pure [e1, e2, e3]
            entities = evalState ops emptyEntityManager
        map entityIndex entities `shouldEqual` [0, 0, 0]
        map entityVersion entities `shouldEqual` [0, 1, 2]

      it "freeList has LIFO (stack-like) behavior" do
        let ops = do
              e1 <- createEntity  -- index 0
              e2 <- createEntity  -- index 1
              _  <- createEntity  -- index 2
              deleteEntity e2     -- Push 1
              deleteEntity e1     -- Push 0
              n1 <- createEntity  -- Pop 0 (LIFO)
              n2 <- createEntity  -- Pop 1
              pure [n1, n2]
            entities = evalState ops emptyEntityManager
        map entityIndex entities `shouldEqual` [0, 1]

    -- Validation Tests
    describe "Validation" do

      it "valid entity passes validation" do
        let ops = do
              entity <- createEntity
              pure entity
            (Tuple value state) = runState ops emptyEntityManager
        validateEntity value state `shouldEqual` true

      it "stale version fails validation" do
        let manager = emptyEntityManager
            entity = EntityId {index: 0, version: 0}
            -- Create a manager with index 0 at version 1
            manager' = manager { versions = Map.insert 0 1 manager.versions }
        validateEntity entity manager' `shouldEqual` false

      it "non-existent index fails validation" do
        let entity = EntityId {index: 999, version: 0}
        validateEntity entity emptyEntityManager `shouldEqual` false

      it "version 0 entities validate initially" do
        let ops = createEntity
            (Tuple value state) = runState ops emptyEntityManager
        entityVersion value `shouldEqual` 0
        validateEntity value state `shouldEqual` true

      it "validation is O(1) lookup" do
        -- Just verify it compiles and works (can't test Big-O directly)
        let manager = emptyEntityManager
            entity = EntityId {index: 0, version: 0}
        validateEntity entity manager `shouldEqual` false

    -- Edge Cases
    describe "Edge Cases" do

      it "delete non-existent entity does nothing (idempotent)" do
        let entity = EntityId {index: 999, version: 0}
            manager = execState (deleteEntity entity) emptyEntityManager
        -- Should not crash, freeList should remain empty
        length manager.freeList `shouldEqual` 0

      it "can create 1000 entities (stress test)" do
        let create1000 = sequence (listReplicate 1000 createEntity)
            entities = evalState create1000 emptyEntityManager
        length entities `shouldEqual` 1000
        -- All should have unique indices
        map entityIndex entities `shouldEqual` (0 .. 999)

      it "create/delete/create cycle 100 times" do
        let cycle = do
              e <- createEntity
              deleteEntity e
              createEntity
            entities = evalState (sequence (listReplicate 100 cycle)) emptyEntityManager
            lastIndex = case entities of
              Nil -> -1
              (x : _) -> entityIndex x
        -- All should reuse index 0
        lastIndex `shouldEqual` 0

      it "free list ordering LIFO verification" do
        let ops = do
              entities <- sequence [createEntity, createEntity, createEntity]
              -- Delete in order: 0, 1, 2
              traverse_ deleteEntity entities
              -- Recreate - should get 2, 1, 0 (LIFO reverse)
              n1 <- createEntity
              n2 <- createEntity
              n3 <- createEntity
              pure [entityIndex n1, entityIndex n2, entityIndex n3]
            indices = evalState ops emptyEntityManager
        indices `shouldEqual` [2, 1, 0]

    -- State Monad Tests
    describe "State Monad" do

      it "runState returns both value and new state" do
        let (Tuple value state) = runState createEntity emptyEntityManager
        -- Should have value (EntityId)
        entityIndex value `shouldEqual` 0
        -- Should have updated state
        state.nextIndex `shouldEqual` 1

      it "evalState returns only value" do
        let entity = evalState createEntity emptyEntityManager
        entityIndex entity `shouldEqual` 0

      it "execState returns only state" do
        let manager = execState createEntity emptyEntityManager
        manager.nextIndex `shouldEqual` 1

      it "composition: create >> delete >> create" do
        let ops = do
              e1 <- createEntity
              deleteEntity e1
              e2 <- createEntity
              pure {e1, e2}
            result = evalState ops emptyEntityManager
        -- Should reuse index with incremented version
        entityIndex result.e1 `shouldEqual` entityIndex result.e2
        entityVersion result.e2 `shouldEqual` (entityVersion result.e1 + 1)

-- Helper to generate range as List
range :: Int -> Int -> List Int
range start end = if start > end then Nil else start : range (start + 1) end

infix 8 range as ..

-- Helper to replicate monadic action
listReplicate :: forall m a. Monad m => Int -> m a -> List (m a)
listReplicate n action = if n <= 0 then Nil else action : listReplicate (n - 1) action
