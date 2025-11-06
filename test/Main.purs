module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ECS.ComponentSpec (componentSpec)
import Test.ECS.EntitySpec (entitySpec)
import Test.ECS.IntegrationSpec (integrationSpec)
import Test.ECS.QuerySpec (querySpec)
import Test.ECS.SystemSpec (systemSpec)
import Test.ECS.WorldSpec (worldSpec)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  entitySpec
  worldSpec
  componentSpec
  querySpec
  systemSpec
  integrationSpec
