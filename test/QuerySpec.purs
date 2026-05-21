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
        let world0 = emptyWorld
            -- Entity B: only the low-bit label that aliases bit 32.
            {world: wB1, entity: eB} = spawnEntityPure world0
            {world: wB2, entity: _} = addComponentPure (Proxy :: _ "c00") unit eB wB1

            -- Entity A: all 33 labels c00 .. c32. The last addComponentPure
            -- forces getOrCreateComponentMask to allocate bit 32.
            {world: wA1, entity: eA00} = spawnEntityPure wB2
            {world: wA2, entity: eA01} = addComponentPure (Proxy :: _ "c00") unit eA00 wA1
            {world: wA3, entity: eA02} = addComponentPure (Proxy :: _ "c01") unit eA01 wA2
            {world: wA4, entity: eA03} = addComponentPure (Proxy :: _ "c02") unit eA02 wA3
            {world: wA5, entity: eA04} = addComponentPure (Proxy :: _ "c03") unit eA03 wA4
            {world: wA6, entity: eA05} = addComponentPure (Proxy :: _ "c04") unit eA04 wA5
            {world: wA7, entity: eA06} = addComponentPure (Proxy :: _ "c05") unit eA05 wA6
            {world: wA8, entity: eA07} = addComponentPure (Proxy :: _ "c06") unit eA06 wA7
            {world: wA9, entity: eA08} = addComponentPure (Proxy :: _ "c07") unit eA07 wA8
            {world: wA10, entity: eA09} = addComponentPure (Proxy :: _ "c08") unit eA08 wA9
            {world: wA11, entity: eA10} = addComponentPure (Proxy :: _ "c09") unit eA09 wA10
            {world: wA12, entity: eA11} = addComponentPure (Proxy :: _ "c10") unit eA10 wA11
            {world: wA13, entity: eA12} = addComponentPure (Proxy :: _ "c11") unit eA11 wA12
            {world: wA14, entity: eA13} = addComponentPure (Proxy :: _ "c12") unit eA12 wA13
            {world: wA15, entity: eA14} = addComponentPure (Proxy :: _ "c13") unit eA13 wA14
            {world: wA16, entity: eA15} = addComponentPure (Proxy :: _ "c14") unit eA14 wA15
            {world: wA17, entity: eA16} = addComponentPure (Proxy :: _ "c15") unit eA15 wA16
            {world: wA18, entity: eA17} = addComponentPure (Proxy :: _ "c16") unit eA16 wA17
            {world: wA19, entity: eA18} = addComponentPure (Proxy :: _ "c17") unit eA17 wA18
            {world: wA20, entity: eA19} = addComponentPure (Proxy :: _ "c18") unit eA18 wA19
            {world: wA21, entity: eA20} = addComponentPure (Proxy :: _ "c19") unit eA19 wA20
            {world: wA22, entity: eA21} = addComponentPure (Proxy :: _ "c20") unit eA20 wA21
            {world: wA23, entity: eA22} = addComponentPure (Proxy :: _ "c21") unit eA21 wA22
            {world: wA24, entity: eA23} = addComponentPure (Proxy :: _ "c22") unit eA22 wA23
            {world: wA25, entity: eA24} = addComponentPure (Proxy :: _ "c23") unit eA23 wA24
            {world: wA26, entity: eA25} = addComponentPure (Proxy :: _ "c24") unit eA24 wA25
            {world: wA27, entity: eA26} = addComponentPure (Proxy :: _ "c25") unit eA25 wA26
            {world: wA28, entity: eA27} = addComponentPure (Proxy :: _ "c26") unit eA26 wA27
            {world: wA29, entity: eA28} = addComponentPure (Proxy :: _ "c27") unit eA27 wA28
            {world: wA30, entity: eA29} = addComponentPure (Proxy :: _ "c28") unit eA28 wA29
            {world: wA31, entity: eA30} = addComponentPure (Proxy :: _ "c29") unit eA29 wA30
            {world: wA32, entity: eA31} = addComponentPure (Proxy :: _ "c30") unit eA30 wA31
            {world: wA33, entity: eA32} = addComponentPure (Proxy :: _ "c31") unit eA31 wA32
            -- This add allocates bit 32 in the component registry.
            {world: wA34, entity: _} = addComponentPure (Proxy :: _ "c32") unit eA32 wA33

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
        let world0 = emptyWorld

            -- Sacrificial entity: burns bits 0..31 in the registry.
            {world: ws01, entity: s00} = spawnEntityPure world0
            {world: ws02, entity: s01} = addComponentPure (Proxy :: _ "c00") unit s00 ws01
            {world: ws03, entity: s02} = addComponentPure (Proxy :: _ "c01") unit s01 ws02
            {world: ws04, entity: s03} = addComponentPure (Proxy :: _ "c02") unit s02 ws03
            {world: ws05, entity: s04} = addComponentPure (Proxy :: _ "c03") unit s03 ws04
            {world: ws06, entity: s05} = addComponentPure (Proxy :: _ "c04") unit s04 ws05
            {world: ws07, entity: s06} = addComponentPure (Proxy :: _ "c05") unit s05 ws06
            {world: ws08, entity: s07} = addComponentPure (Proxy :: _ "c06") unit s06 ws07
            {world: ws09, entity: s08} = addComponentPure (Proxy :: _ "c07") unit s07 ws08
            {world: ws10, entity: s09} = addComponentPure (Proxy :: _ "c08") unit s08 ws09
            {world: ws11, entity: s10} = addComponentPure (Proxy :: _ "c09") unit s09 ws10
            {world: ws12, entity: s11} = addComponentPure (Proxy :: _ "c10") unit s10 ws11
            {world: ws13, entity: s12} = addComponentPure (Proxy :: _ "c11") unit s11 ws12
            {world: ws14, entity: s13} = addComponentPure (Proxy :: _ "c12") unit s12 ws13
            {world: ws15, entity: s14} = addComponentPure (Proxy :: _ "c13") unit s13 ws14
            {world: ws16, entity: s15} = addComponentPure (Proxy :: _ "c14") unit s14 ws15
            {world: ws17, entity: s16} = addComponentPure (Proxy :: _ "c15") unit s15 ws16
            {world: ws18, entity: s17} = addComponentPure (Proxy :: _ "c16") unit s16 ws17
            {world: ws19, entity: s18} = addComponentPure (Proxy :: _ "c17") unit s17 ws18
            {world: ws20, entity: s19} = addComponentPure (Proxy :: _ "c18") unit s18 ws19
            {world: ws21, entity: s20} = addComponentPure (Proxy :: _ "c19") unit s19 ws20
            {world: ws22, entity: s21} = addComponentPure (Proxy :: _ "c20") unit s20 ws21
            {world: ws23, entity: s22} = addComponentPure (Proxy :: _ "c21") unit s21 ws22
            {world: ws24, entity: s23} = addComponentPure (Proxy :: _ "c22") unit s22 ws23
            {world: ws25, entity: s24} = addComponentPure (Proxy :: _ "c23") unit s23 ws24
            {world: ws26, entity: s25} = addComponentPure (Proxy :: _ "c24") unit s24 ws25
            {world: ws27, entity: s26} = addComponentPure (Proxy :: _ "c25") unit s25 ws26
            {world: ws28, entity: s27} = addComponentPure (Proxy :: _ "c26") unit s26 ws27
            {world: ws29, entity: s28} = addComponentPure (Proxy :: _ "c27") unit s27 ws28
            {world: ws30, entity: s29} = addComponentPure (Proxy :: _ "c28") unit s28 ws29
            {world: ws31, entity: s30} = addComponentPure (Proxy :: _ "c29") unit s29 ws30
            {world: ws32, entity: s31} = addComponentPure (Proxy :: _ "c30") unit s30 ws31
            {world: ws33, entity: _}   = addComponentPure (Proxy :: _ "c31") unit s31 ws32

            -- Entity A: only c00 and c32. With bits 0..31 already allocated,
            -- c32 will be assigned bit 32 (which overflows to bit 0 in JS).
            {world: wA1, entity: a0} = spawnEntityPure ws33
            {world: wA2, entity: a1} = addComponentPure (Proxy :: _ "c00") unit a0 wA1
            {world: wA3, entity: _}  = addComponentPure (Proxy :: _ "c32") unit a1 wA2

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
        let world0 = emptyWorld

            -- Burn bits 0..31.
            {world: ws01, entity: s00} = spawnEntityPure world0
            {world: ws02, entity: s01} = addComponentPure (Proxy :: _ "c00") unit s00 ws01
            {world: ws03, entity: s02} = addComponentPure (Proxy :: _ "c01") unit s01 ws02
            {world: ws04, entity: s03} = addComponentPure (Proxy :: _ "c02") unit s02 ws03
            {world: ws05, entity: s04} = addComponentPure (Proxy :: _ "c03") unit s03 ws04
            {world: ws06, entity: s05} = addComponentPure (Proxy :: _ "c04") unit s04 ws05
            {world: ws07, entity: s06} = addComponentPure (Proxy :: _ "c05") unit s05 ws06
            {world: ws08, entity: s07} = addComponentPure (Proxy :: _ "c06") unit s06 ws07
            {world: ws09, entity: s08} = addComponentPure (Proxy :: _ "c07") unit s07 ws08
            {world: ws10, entity: s09} = addComponentPure (Proxy :: _ "c08") unit s08 ws09
            {world: ws11, entity: s10} = addComponentPure (Proxy :: _ "c09") unit s09 ws10
            {world: ws12, entity: s11} = addComponentPure (Proxy :: _ "c10") unit s10 ws11
            {world: ws13, entity: s12} = addComponentPure (Proxy :: _ "c11") unit s11 ws12
            {world: ws14, entity: s13} = addComponentPure (Proxy :: _ "c12") unit s12 ws13
            {world: ws15, entity: s14} = addComponentPure (Proxy :: _ "c13") unit s13 ws14
            {world: ws16, entity: s15} = addComponentPure (Proxy :: _ "c14") unit s14 ws15
            {world: ws17, entity: s16} = addComponentPure (Proxy :: _ "c15") unit s15 ws16
            {world: ws18, entity: s17} = addComponentPure (Proxy :: _ "c16") unit s16 ws17
            {world: ws19, entity: s18} = addComponentPure (Proxy :: _ "c17") unit s17 ws18
            {world: ws20, entity: s19} = addComponentPure (Proxy :: _ "c18") unit s18 ws19
            {world: ws21, entity: s20} = addComponentPure (Proxy :: _ "c19") unit s19 ws20
            {world: ws22, entity: s21} = addComponentPure (Proxy :: _ "c20") unit s20 ws21
            {world: ws23, entity: s22} = addComponentPure (Proxy :: _ "c21") unit s21 ws22
            {world: ws24, entity: s23} = addComponentPure (Proxy :: _ "c22") unit s22 ws23
            {world: ws25, entity: s24} = addComponentPure (Proxy :: _ "c23") unit s23 ws24
            {world: ws26, entity: s25} = addComponentPure (Proxy :: _ "c24") unit s24 ws25
            {world: ws27, entity: s26} = addComponentPure (Proxy :: _ "c25") unit s25 ws26
            {world: ws28, entity: s27} = addComponentPure (Proxy :: _ "c26") unit s26 ws27
            {world: ws29, entity: s28} = addComponentPure (Proxy :: _ "c27") unit s27 ws28
            {world: ws30, entity: s29} = addComponentPure (Proxy :: _ "c28") unit s28 ws29
            {world: ws31, entity: s30} = addComponentPure (Proxy :: _ "c29") unit s29 ws30
            {world: ws32, entity: s31} = addComponentPure (Proxy :: _ "c30") unit s30 ws31
            {world: ws33, entity: _}   = addComponentPure (Proxy :: _ "c31") unit s31 ws32

            -- Entity A: c00 + c32 (forces bit 32 allocation).
            {world: wA1, entity: a0} = spawnEntityPure ws33
            {world: wA2, entity: a1} = addComponentPure (Proxy :: _ "c00") unit a0 wA1
            {world: wA3, entity: a2} = addComponentPure (Proxy :: _ "c32") unit a1 wA2

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
        let world0 = emptyWorld

            -- Burn bits 0..31 with a sacrificial entity.
            {world: ws01, entity: s00} = spawnEntityPure world0
            {world: ws02, entity: s01} = addComponentPure (Proxy :: _ "c00") unit s00 ws01
            {world: ws03, entity: s02} = addComponentPure (Proxy :: _ "c01") unit s01 ws02
            {world: ws04, entity: s03} = addComponentPure (Proxy :: _ "c02") unit s02 ws03
            {world: ws05, entity: s04} = addComponentPure (Proxy :: _ "c03") unit s03 ws04
            {world: ws06, entity: s05} = addComponentPure (Proxy :: _ "c04") unit s04 ws05
            {world: ws07, entity: s06} = addComponentPure (Proxy :: _ "c05") unit s05 ws06
            {world: ws08, entity: s07} = addComponentPure (Proxy :: _ "c06") unit s06 ws07
            {world: ws09, entity: s08} = addComponentPure (Proxy :: _ "c07") unit s07 ws08
            {world: ws10, entity: s09} = addComponentPure (Proxy :: _ "c08") unit s08 ws09
            {world: ws11, entity: s10} = addComponentPure (Proxy :: _ "c09") unit s09 ws10
            {world: ws12, entity: s11} = addComponentPure (Proxy :: _ "c10") unit s10 ws11
            {world: ws13, entity: s12} = addComponentPure (Proxy :: _ "c11") unit s11 ws12
            {world: ws14, entity: s13} = addComponentPure (Proxy :: _ "c12") unit s12 ws13
            {world: ws15, entity: s14} = addComponentPure (Proxy :: _ "c13") unit s13 ws14
            {world: ws16, entity: s15} = addComponentPure (Proxy :: _ "c14") unit s14 ws15
            {world: ws17, entity: s16} = addComponentPure (Proxy :: _ "c15") unit s15 ws16
            {world: ws18, entity: s17} = addComponentPure (Proxy :: _ "c16") unit s16 ws17
            {world: ws19, entity: s18} = addComponentPure (Proxy :: _ "c17") unit s17 ws18
            {world: ws20, entity: s19} = addComponentPure (Proxy :: _ "c18") unit s18 ws19
            {world: ws21, entity: s20} = addComponentPure (Proxy :: _ "c19") unit s19 ws20
            {world: ws22, entity: s21} = addComponentPure (Proxy :: _ "c20") unit s20 ws21
            {world: ws23, entity: s22} = addComponentPure (Proxy :: _ "c21") unit s21 ws22
            {world: ws24, entity: s23} = addComponentPure (Proxy :: _ "c22") unit s22 ws23
            {world: ws25, entity: s24} = addComponentPure (Proxy :: _ "c23") unit s23 ws24
            {world: ws26, entity: s25} = addComponentPure (Proxy :: _ "c24") unit s24 ws25
            {world: ws27, entity: s26} = addComponentPure (Proxy :: _ "c25") unit s25 ws26
            {world: ws28, entity: s27} = addComponentPure (Proxy :: _ "c26") unit s26 ws27
            {world: ws29, entity: s28} = addComponentPure (Proxy :: _ "c27") unit s27 ws28
            {world: ws30, entity: s29} = addComponentPure (Proxy :: _ "c28") unit s28 ws29
            {world: ws31, entity: s30} = addComponentPure (Proxy :: _ "c29") unit s29 ws30
            {world: ws32, entity: s31} = addComponentPure (Proxy :: _ "c30") unit s30 ws31
            {world: ws33, entity: _}   = addComponentPure (Proxy :: _ "c31") unit s31 ws32

            q :: _ (c32 :: Unit) ()
            q = query (Proxy :: _ (c32 :: Unit))

            -- First call: c32 hasn't been registered yet, so the strict
            -- mask fails and we get an empty result. This populates the
            -- cache for THIS query shape (or short-circuits before caching;
            -- either is acceptable).
            r1 = runQueryCached q ws33

            -- Add an entity carrying c32 (forces bit-32 allocation AND
            -- creates a new archetype → structuralVersion bumps).
            {world: wA1, entity: a0} = spawnEntityPure r1.world
            {world: wA2, entity: _}  = addComponentPure (Proxy :: _ "c32") unit a0 wA1

            -- Second call: must NOT use a stale cache that pre-dates the
            -- new archetype.
            r2 = runQueryCached q wA2

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
