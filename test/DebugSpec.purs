module Test.ECS.DebugSpec where

import Prelude

import Data.Set as Set
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import ECS.Component (addComponentPure)
import ECS.Debug (dumpWorld)
import ECS.World (World, emptyWorld, spawnEntityPure)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

-- Test component types
type Position = { x :: Number, y :: Number }
type Velocity = { x :: Number, y :: Number }
type Health = { current :: Int, max :: Int }

_position :: Proxy "position"
_position = Proxy

_velocity :: Proxy "velocity"
_velocity = Proxy

_health :: Proxy "health"
_health = Proxy

-- | Helper: assert a substring is present in the dump text.
shouldContain :: String -> String -> Aff Unit
shouldContain haystack needle =
  contains (Pattern needle) haystack `shouldEqual` true

debugSpec :: Spec Unit
debugSpec = do
  describe "ECS.Debug" do

    describe "dumpWorld" do

      it "renders header, registry, archetypes, locations, cache sections on a small hero world" do
        let
          w0 = emptyWorld
          { world: w1, entity: hero0 } = spawnEntityPure w0
          { world: w2, entity: hero1 } = addComponentPure _position { x: 1.0, y: 2.0 } hero0 w1
          { world: w3, entity: hero2 } = addComponentPure _velocity { x: 0.0, y: -1.0 } hero1 w2
          { world: w4, entity: _ } = addComponentPure _health { current: 10, max: 10 } hero2 w3
          dump = dumpWorld w4

        shouldContain dump "World:"
        shouldContain dump "entities"
        shouldContain dump "archetypes"
        shouldContain dump "registered labels"
        shouldContain dump "ComponentRegistry:"
        shouldContain dump "bit 0:"
        shouldContain dump "\"position\""
        shouldContain dump "\"velocity\""
        shouldContain dump "\"health\""
        shouldContain dump "Archetypes:"
        shouldContain dump "mask:"
        shouldContain dump "labels:"
        shouldContain dump "popcount="
        shouldContain dump "storage columns:"
        shouldContain dump "position:"
        shouldContain dump "EntityLocations:"
        shouldContain dump "entities mapped"
        shouldContain dump "QueryCache:"
        shouldContain dump "fresh="
        shouldContain dump "stale="

      it "flags DIVERGENCE when an archetype's labels Set is inconsistent with its mask" do
        -- Build a real world, then surgically corrupt one archetype so its
        -- labels Set claims a component the mask + storage don't carry.
        -- This is impossible to reach through the public API, but is exactly
        -- the kind of latent corruption dumpWorld is meant to surface.
        let
          w0 = emptyWorld
          { world: w1, entity: e1 } = spawnEntityPure w0
          { world: w2, entity: _ } = addComponentPure _position { x: 1.0, y: 2.0 } e1 w1
          corrupted :: World
          corrupted = w2
            { archetypes =
                map
                  (\arch ->
                    if Set.member "position" arch.labels
                      then arch { labels = Set.insert "ghost" arch.labels }
                      else arch
                  )
                  w2.archetypes
            }
          dump = dumpWorld corrupted
        shouldContain dump "DIVERGENCE"
        shouldContain dump "ghost"
