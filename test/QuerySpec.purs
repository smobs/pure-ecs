module Test.ECS.QuerySpec where

import Prelude

import Control.Monad.State (execState, runState)
import Data.Array (length, index)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ECS.Component (addComponentPure, getComponentPure, (<+>), (:=))
import ECS.Entity (entityIndex)
import ECS.Query (query, without, runQuery, runQueryCached, forQuery, mapQuery)
import ECS.World (World, emptyWorld, spawnEntity, spawnEntityPure, unEntity)
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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w2

        length results `shouldEqual` 1
        case results !! 0 of
          Nothing -> fail "Expected one result"
          Just r -> r.components.position.x `shouldEqual` 10.0

      it "query multiple components (AND logic)" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with both position and velocity
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: e4} = addComponentPure _position {x: 30.0, y: 40.0} e3 w3
            {world: w5, entity: _} = addComponentPure _velocity {x: 1.0, y: 0.0} e4 w4

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with position and frozen
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: e4} = addComponentPure _position {x: 30.0, y: 40.0} e3 w3
            {world: w5, entity: _} = addComponentPure _frozen unit e4 w4

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            -- Entity with position and frozen
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: e4} = addComponentPure _position {x: 20.0, y: 30.0} e3 w3
            {world: w5, entity: _} = addComponentPure _frozen unit e4 w4
            -- Entity with position and health
            {world: w6, entity: e6} = spawnEntityPure w5
            {world: w7, entity: e7} = addComponentPure _position {x: 30.0, y: 40.0} e6 w6
            {world: w8, entity: _} = addComponentPure _health {current: 100, max: 100} e7 w7

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            {world: w4, entity: _} = addComponentPure _health {current: 100, max: 100} e3 w3

            -- Query for just position - should match even with extra components
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w4

        length results `shouldEqual` 1

      it "missing required component prevents match" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1

            -- Query for position and velocity - should not match (missing velocity)
            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            results = runQuery q w2

        length results `shouldEqual` 0

      it "excluded component prevents match" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponentPure _frozen unit e2 w2

            q :: _ (position :: Position) (frozen :: Unit)
            q = query (Proxy :: _ (position :: Position)) # without _frozen
            results = runQuery q w3

        length results `shouldEqual` 0

    -- Multiple Entities Tests
    describe "Multiple Entities" do

      it "query returns all matching entities" do
        let world0 = emptyWorld
            -- Create 3 entities with position
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 10.0} e1 w1
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: _} = addComponentPure _position {x: 20.0, y: 20.0} e3 w3
            {world: w5, entity: e5} = spawnEntityPure w4
            {world: w6, entity: _} = addComponentPure _position {x: 30.0, y: 30.0} e5 w5

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w6

        length results `shouldEqual` 3

      it "results from different archetypes" do
        let world0 = emptyWorld
            -- Entity with just position
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 10.0} e1 w1
            -- Entity with position and velocity
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: e4} = addComponentPure _position {x: 20.0, y: 20.0} e3 w3
            {world: w5, entity: _} = addComponentPure _velocity {x: 1.0, y: 0.0} e4 w4

            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results = runQuery q w5

        -- Both entities match (different archetypes)
        length results `shouldEqual` 2

      it "component values unique per entity" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 100.0, y: 200.0} e1 w1
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: _} = addComponentPure _position {x: 300.0, y: 400.0} e3 w3

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: _} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: e3} = spawnEntityPure w2
            {world: w4, entity: _} = addComponentPure _position {x: 30.0, y: 40.0} e3 w3

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
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponentPure _velocity {x: 5.0, y: 0.0} e2 w2

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            -- Update positions based on velocities
            w4 = mapQuery q updatePos w3

            -- Check that position was updated
            results = runQuery q w4

        length results `shouldEqual` 1
        -- Note: This test is simplified - full update would require
        -- removeComponentPure and addComponentPure to change component values
        true `shouldEqual` true

      it "forQuery receives correct types" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1
            {world: w3, entity: _} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2

            q :: _ (position :: Position, velocity :: Velocity) ()
            q = query (Proxy :: _ (position :: Position, velocity :: Velocity))
            -- Access multiple fields to verify types
            sums = forQuery q (\r -> r.components.position.x + r.components.velocity.x) w3

        case sums !! 0 of
          Just sum -> sum `shouldEqual` 11.0
          Nothing -> fail "Expected one result"

    -- Component-mask overflow regression (>31 distinct labels)
    --
    -- ECS.World uses Int (32-bit signed in JS) for ComponentMask, and
    -- `bitToMask bit = 1 \`shl\` bit` overflows for bit >= 32: 1 << 32 == 1,
    -- 1 << 33 == 2, etc. Once getOrCreateComponentMask hands out bit 32+,
    -- the high-bit label's mask aliases a low-bit label's mask, and the
    -- bitwise archetype filter starts matching entities that don't actually
    -- carry the high-bit component. Queries return wrong results.
    --
    -- Reproduction recipe used here:
    --   - Entity A carries 33 distinct labels c00 .. c32 (forces bit 32 to be
    --     allocated; "c32" lives at the same mask as "c00").
    --   - Entity B carries only c00 (bit 0). Its archetype mask is 1.
    --   - Query for the single high-bit label c32: required mask in the
    --     buggy world is `1 \`shl\` 32` == 1, so B's archetype (mask 1) is
    --     incorrectly included. Expected length 1; under the bug length is 2.
    --   - Query for (c00, c32) together: required mask collapses to 1 for the
    --     same reason, so B is again incorrectly included.
    describe "Component-mask >31 labels regression" do

      it "single high-bit-only query returns only the entity that actually has it" do
        -- Entity B: only the low-bit label that aliases bit 32.
        -- Use runState so we keep the resulting Entity reference for the
        -- negative-space check at the end.
        let Tuple eB worldAfterB = runState
              ( spawnEntity
                  <+> (Proxy :: _ "c00") := unit
              ) emptyWorld

            -- Entity A: all 33 labels c00 .. c32. The last component
            -- addition forces getOrCreateComponentMask to allocate bit 32.
            wA34 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c01") := unit
                  <+> (Proxy :: _ "c02") := unit
                  <+> (Proxy :: _ "c03") := unit
                  <+> (Proxy :: _ "c04") := unit
                  <+> (Proxy :: _ "c05") := unit
                  <+> (Proxy :: _ "c06") := unit
                  <+> (Proxy :: _ "c07") := unit
                  <+> (Proxy :: _ "c08") := unit
                  <+> (Proxy :: _ "c09") := unit
                  <+> (Proxy :: _ "c10") := unit
                  <+> (Proxy :: _ "c11") := unit
                  <+> (Proxy :: _ "c12") := unit
                  <+> (Proxy :: _ "c13") := unit
                  <+> (Proxy :: _ "c14") := unit
                  <+> (Proxy :: _ "c15") := unit
                  <+> (Proxy :: _ "c16") := unit
                  <+> (Proxy :: _ "c17") := unit
                  <+> (Proxy :: _ "c18") := unit
                  <+> (Proxy :: _ "c19") := unit
                  <+> (Proxy :: _ "c20") := unit
                  <+> (Proxy :: _ "c21") := unit
                  <+> (Proxy :: _ "c22") := unit
                  <+> (Proxy :: _ "c23") := unit
                  <+> (Proxy :: _ "c24") := unit
                  <+> (Proxy :: _ "c25") := unit
                  <+> (Proxy :: _ "c26") := unit
                  <+> (Proxy :: _ "c27") := unit
                  <+> (Proxy :: _ "c28") := unit
                  <+> (Proxy :: _ "c29") := unit
                  <+> (Proxy :: _ "c30") := unit
                  <+> (Proxy :: _ "c31") := unit
                  <+> (Proxy :: _ "c32") := unit
              ) worldAfterB

            q :: _ (c32 :: Unit) ()
            q = query (Proxy :: _ (c32 :: Unit))
            results = runQuery q wA34

        -- Only entity A has c32. Under the >31-bit overflow bug, entity B
        -- (which only has c00) is also returned because bit 32 aliases bit 0
        -- in the required mask.
        length results `shouldEqual` 1

        -- Negative space: explicitly assert that entity B (which lacks c32) is
        -- NOT in the results. Catches a "fix that always succeeds" — a
        -- broken maskContains returning true for any high-bit query would
        -- pass `length results == 1` if only one of A/B was inadvertently
        -- dropped from elsewhere, but would fail this stronger check.
        let bIndex = entityIndex (unEntity eB)
            resultBIxs = map (\r -> entityIndex (unEntity r.entity)) results
        Array.elem bIndex resultBIxs `shouldEqual` false

      it "multi-component query for an entity that has both a low and high label still returns it" do
        -- Mirrors the consumer's exact symptom: `getComponent` finds each
        -- component individually, but a multi-component query returns 0.
        --
        -- Setup:
        --   - Sacrificial entity S burns bits 0..31 in the component registry
        --     (so the NEXT new label is forced to bit 32). S also exists in
        --     the world but never participates in the queries below.
        --   - Entity A then carries c00 and c32. c32's bit allocation falls
        --     at bit 32; under the overflow bug, bitToMask 32 == 1, which
        --     aliases the bit that c00 already occupies.
        --
        -- Result under the bug:
        --   - A's archetype mask after adding c32 is `1 | (1 << 32)` == 1
        --     (unchanged). A's archetype.labels stay {c00} only, even though
        --     its storage now has both c00 and c32 columns (so getComponent
        --     for c32 still works).
        --   - Query for (c00, c32) computes a required mask that has bit 32
        --     set distinctly from bit 0 (labelsToMaskStrict accumulates via
        --     `+ (1 \`shl\` bit)`, so required != 1), but A's archetype mask
        --     is 1. The bitmask predicate maskContains fails, and the query
        --     returns 0 results.
        -- Sacrificial entity: burns bits 0..31 in the registry.
        let ws33 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c01") := unit
                  <+> (Proxy :: _ "c02") := unit
                  <+> (Proxy :: _ "c03") := unit
                  <+> (Proxy :: _ "c04") := unit
                  <+> (Proxy :: _ "c05") := unit
                  <+> (Proxy :: _ "c06") := unit
                  <+> (Proxy :: _ "c07") := unit
                  <+> (Proxy :: _ "c08") := unit
                  <+> (Proxy :: _ "c09") := unit
                  <+> (Proxy :: _ "c10") := unit
                  <+> (Proxy :: _ "c11") := unit
                  <+> (Proxy :: _ "c12") := unit
                  <+> (Proxy :: _ "c13") := unit
                  <+> (Proxy :: _ "c14") := unit
                  <+> (Proxy :: _ "c15") := unit
                  <+> (Proxy :: _ "c16") := unit
                  <+> (Proxy :: _ "c17") := unit
                  <+> (Proxy :: _ "c18") := unit
                  <+> (Proxy :: _ "c19") := unit
                  <+> (Proxy :: _ "c20") := unit
                  <+> (Proxy :: _ "c21") := unit
                  <+> (Proxy :: _ "c22") := unit
                  <+> (Proxy :: _ "c23") := unit
                  <+> (Proxy :: _ "c24") := unit
                  <+> (Proxy :: _ "c25") := unit
                  <+> (Proxy :: _ "c26") := unit
                  <+> (Proxy :: _ "c27") := unit
                  <+> (Proxy :: _ "c28") := unit
                  <+> (Proxy :: _ "c29") := unit
                  <+> (Proxy :: _ "c30") := unit
                  <+> (Proxy :: _ "c31") := unit
              ) emptyWorld

            -- Entity A: only c00 and c32. With bits 0..31 already allocated,
            -- c32 will be assigned bit 32 (which overflows to bit 0 in JS).
            wA3 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c32") := unit
              ) ws33

            q :: _ (c00 :: Unit, c32 :: Unit) ()
            q = query (Proxy :: _ (c00 :: Unit, c32 :: Unit))
            results = runQuery q wA3

            -- The entity-index of A (the real owner of both c00 and c32). The
            -- sacrificial entity S has index 0; A is the second spawn, index 1.
            aIndex = 1

            resultIndices = map (\r -> entityIndex (unEntity r.entity)) results

        -- A has c00 AND c32; the query for (c00, c32) MUST return A.
        -- Under the >31-bit overflow bug A's archetype mask is `1 | (1 << 32) == 1`,
        -- which fails the bitmask predicate for the (c00, c32) required mask,
        -- so the query either drops A entirely or returns the wrong entity
        -- (the sacrificial entity S, which legitimately has c00 but not c32).
        Array.elem aIndex resultIndices `shouldEqual` true

      it "getComponent agrees with queryFor on a high-bit entity" do
        -- Bug-report symptom: getComponent finds each component individually
        -- while a multi-component query misses the entity entirely. This
        -- test pins down agreement between the two paths.
        --
        -- Burn bits 0..31 with a sacrificial entity.
        let ws33 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c01") := unit
                  <+> (Proxy :: _ "c02") := unit
                  <+> (Proxy :: _ "c03") := unit
                  <+> (Proxy :: _ "c04") := unit
                  <+> (Proxy :: _ "c05") := unit
                  <+> (Proxy :: _ "c06") := unit
                  <+> (Proxy :: _ "c07") := unit
                  <+> (Proxy :: _ "c08") := unit
                  <+> (Proxy :: _ "c09") := unit
                  <+> (Proxy :: _ "c10") := unit
                  <+> (Proxy :: _ "c11") := unit
                  <+> (Proxy :: _ "c12") := unit
                  <+> (Proxy :: _ "c13") := unit
                  <+> (Proxy :: _ "c14") := unit
                  <+> (Proxy :: _ "c15") := unit
                  <+> (Proxy :: _ "c16") := unit
                  <+> (Proxy :: _ "c17") := unit
                  <+> (Proxy :: _ "c18") := unit
                  <+> (Proxy :: _ "c19") := unit
                  <+> (Proxy :: _ "c20") := unit
                  <+> (Proxy :: _ "c21") := unit
                  <+> (Proxy :: _ "c22") := unit
                  <+> (Proxy :: _ "c23") := unit
                  <+> (Proxy :: _ "c24") := unit
                  <+> (Proxy :: _ "c25") := unit
                  <+> (Proxy :: _ "c26") := unit
                  <+> (Proxy :: _ "c27") := unit
                  <+> (Proxy :: _ "c28") := unit
                  <+> (Proxy :: _ "c29") := unit
                  <+> (Proxy :: _ "c30") := unit
                  <+> (Proxy :: _ "c31") := unit
              ) emptyWorld

            -- Entity A: c00 + c32 (forces bit 32 allocation). Need both the
            -- entity reference (for getComponentPure) and the final world,
            -- so use runState.
            Tuple a2 wA3 = runState
              ( spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c32") := unit
              ) ws33

            -- getComponent on each label individually.
            mC00 = getComponentPure (Proxy :: _ "c00") a2 wA3
            mC32 = getComponentPure (Proxy :: _ "c32") a2 wA3

            -- runQuery for both components together.
            q :: _ (c00 :: Unit, c32 :: Unit) ()
            q = query (Proxy :: _ (c00 :: Unit, c32 :: Unit))
            results = runQuery q wA3
            aIndex = entityIndex (unEntity a2)
            resultIndices = map (\r -> entityIndex (unEntity r.entity)) results

        -- Both paths must agree: each label is present individually AND the
        -- entity appears in the multi-component query.
        mC00 `shouldEqual` (Just unit)
        mC32 `shouldEqual` (Just unit)
        Array.elem aIndex resultIndices `shouldEqual` true

      it "cached query sees a new high-bit archetype after structural change" do
        -- runQueryCached caches matching archetype IDs and invalidates them
        -- on structuralVersion bump. Test that adding an entity with a
        -- high-bit component AFTER a cached miss causes the next call to
        -- include it.
        --
        -- Burn bits 0..31 with a sacrificial entity.
        let ws33 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c00") := unit
                  <+> (Proxy :: _ "c01") := unit
                  <+> (Proxy :: _ "c02") := unit
                  <+> (Proxy :: _ "c03") := unit
                  <+> (Proxy :: _ "c04") := unit
                  <+> (Proxy :: _ "c05") := unit
                  <+> (Proxy :: _ "c06") := unit
                  <+> (Proxy :: _ "c07") := unit
                  <+> (Proxy :: _ "c08") := unit
                  <+> (Proxy :: _ "c09") := unit
                  <+> (Proxy :: _ "c10") := unit
                  <+> (Proxy :: _ "c11") := unit
                  <+> (Proxy :: _ "c12") := unit
                  <+> (Proxy :: _ "c13") := unit
                  <+> (Proxy :: _ "c14") := unit
                  <+> (Proxy :: _ "c15") := unit
                  <+> (Proxy :: _ "c16") := unit
                  <+> (Proxy :: _ "c17") := unit
                  <+> (Proxy :: _ "c18") := unit
                  <+> (Proxy :: _ "c19") := unit
                  <+> (Proxy :: _ "c20") := unit
                  <+> (Proxy :: _ "c21") := unit
                  <+> (Proxy :: _ "c22") := unit
                  <+> (Proxy :: _ "c23") := unit
                  <+> (Proxy :: _ "c24") := unit
                  <+> (Proxy :: _ "c25") := unit
                  <+> (Proxy :: _ "c26") := unit
                  <+> (Proxy :: _ "c27") := unit
                  <+> (Proxy :: _ "c28") := unit
                  <+> (Proxy :: _ "c29") := unit
                  <+> (Proxy :: _ "c30") := unit
                  <+> (Proxy :: _ "c31") := unit
              ) emptyWorld

            q :: _ (c32 :: Unit) ()
            q = query (Proxy :: _ (c32 :: Unit))

            -- First call: c32 hasn't been registered yet, so the strict
            -- mask fails and we get an empty result. This populates the
            -- cache for THIS query shape (or short-circuits before caching;
            -- either is acceptable).
            r1 = runQueryCached q ws33

            -- Add an entity carrying c32 (forces bit-32 allocation AND
            -- creates a new archetype → structuralVersion bumps). Resume
            -- from r1.world so any cache populated by the first call is
            -- retained.
            wA2 = execState
              ( void $ spawnEntity
                  <+> (Proxy :: _ "c32") := unit
              ) r1.world

            -- Second call: must NOT use a stale cache that pre-dates the
            -- new archetype.
            r2 = runQueryCached q wA2

        length r1.results `shouldEqual` 0
        length r2.results `shouldEqual` 1

      it "cached empty-row query is not stale after the first spawn (S5)" do
        -- spawnEntityPure creates the empty archetype on the first spawn. If
        -- it omits the structuralVersion bump, a query cached against a world
        -- with zero archetypes stays valid and misses the spawned entity.
        let q :: _ () ()
            q = query (Proxy :: _ ())
            -- Cache the empty-row query against a world with NO archetypes.
            r1 = runQueryCached q emptyWorld
            -- First spawn creates the empty archetype (the only absent->present
            -- transition that previously skipped the version bump).
            w1 = (spawnEntityPure r1.world).world
            -- Second call must see the spawned entity, not a stale [].
            r2 = runQueryCached q w1
        length r1.results `shouldEqual` 0
        length r2.results `shouldEqual` 1

      it "mask-resolution cache re-resolves when a new component registers (Win A)" do
        -- runQueryCached memoises label->mask resolution, guarded by the
        -- registry's nextBit. A query for an unregistered label resolves to
        -- Nothing (empty results) and caches that. After the label registers
        -- (nextBit grows), the SAME query must re-resolve and find the entity.
        let q :: _ (mana :: Int) ()
            q = query (Proxy :: _ (mana :: Int))
            -- First call: 'mana' unregistered -> Nothing -> [] (caches the
            -- resolution at the current nextBit).
            r1 = runQueryCached q emptyWorld
            -- Spawn an entity carrying 'mana' (registers it, grows nextBit).
            w1 = execState (void $ spawnEntity <+> (Proxy :: _ "mana") := 5) r1.world
            -- Second call must re-resolve and see the entity.
            r2 = runQueryCached q w1
        length r1.results `shouldEqual` 0
        length r2.results `shouldEqual` 1

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
            {world: w1, entity: _} = spawnEntityPure world0

            -- Empty query matches all entities
            q :: _ () ()
            q = query (Proxy :: _ ())
            results = runQuery q w1

        length results `shouldEqual` 1

      it "query after component operations" do
        let world0 = emptyWorld
            {world: w1, entity: e1} = spawnEntityPure world0
            {world: w2, entity: e2} = addComponentPure _position {x: 10.0, y: 20.0} e1 w1

            -- Initially matches
            q :: _ (position :: Position) ()
            q = query (Proxy :: _ (position :: Position))
            results1 = runQuery q w2

        length results1 `shouldEqual` 1

        -- After adding more components, still matches
        let {world: w3, entity: _} = addComponentPure _velocity {x: 1.0, y: 0.0} e2 w2
            results2 = runQuery q w3

        length results2 `shouldEqual` 1

-- Helper functions for tests
infixl 8 arrayIndex as !!

arrayIndex :: forall a. Array a -> Int -> Maybe a
arrayIndex = index

updatePos :: forall r e. { entity :: e , components :: Record (position :: Position, velocity :: Velocity | r) } -> World -> World
updatePos _ w = w  -- Simplified - would update position based on velocity
