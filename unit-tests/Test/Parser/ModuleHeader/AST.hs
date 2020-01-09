{-# LANGUAGE TemplateHaskell #-}
module Test.Parser.ModuleHeader.AST
  ( tests
  ) where

import           Prelude
import qualified Data.Set as Set (fromList, singleton)
import qualified Data.Map.Strict as Map (fromList)
import           Hedgehog (Property)
import           Hedgehog (annotate, checkParallel, discover, failure, footnoteShow)
import           Hedgehog ((===))
import           Test.Util (exampleProperty)

import           Text.Trifecta (Spanned(..), Span(..))
import           Text.Trifecta.Delta (Delta(..))
import           Parser.ModuleHeader.AST (ModuleHeader(..), ModuleName(..), DependencyCycles(..))
import           Parser.ModuleHeader.AST (ProperName, QualifiedName(..), Import(..), HaskellImport(..))
import           Parser.ModuleHeader.AST (dependencyList)

emptySpan :: Span
emptySpan = Span (Columns 0 0) (Columns 0 0) ""

simpleModule :: ProperName -> ModuleName
simpleModule = ModuleName . QualifiedName []

haskellImport :: ModuleName -> Spanned Import
haskellImport name = MkHaskellImport (HaskellImport name [] Nothing) :~ emptySpan

prop_check_empty :: Property
prop_check_empty = exampleProperty do
  dependencyList [] === Right []

prop_check_simple :: Property
prop_check_simple = exampleProperty do
  let
    aName = simpleModule "A"
    bName = simpleModule "B"
    a = ModuleHeader (aName :~ emptySpan) [] []
    b = ModuleHeader (bName :~ emptySpan) [] []
  dependencyList [a, b] === Right [ Set.fromList [ aName, bName ] ]

prop_check_simple_deps :: Property
prop_check_simple_deps = exampleProperty do
  let
    aName = simpleModule "A"
    bName = simpleModule "B"
    a = ModuleHeader (aName :~ emptySpan) [] [haskellImport bName]
    b = ModuleHeader (bName :~ emptySpan) [] []
  dependencyList [a, b] === Right [ Set.singleton bName, Set.singleton aName ]

prop_check_simple_deps_2 :: Property
prop_check_simple_deps_2 = exampleProperty do
  let
    aName = simpleModule "A"
    bName = simpleModule "B"
    cName = simpleModule "C"
    dName = simpleModule "D"
    a = ModuleHeader (aName :~ emptySpan) [] [haskellImport bName, haskellImport cName]
    b = ModuleHeader (bName :~ emptySpan) [] [haskellImport dName]
    c = ModuleHeader (cName :~ emptySpan) [] [haskellImport dName]
    d = ModuleHeader (dName :~ emptySpan) [] []
  dependencyList [a, b, c, d] === Right
    [ Set.singleton dName
    , Set.fromList [ bName, cName ]
    , Set.singleton aName
    ]

prop_check_simple_deps_3 :: Property
prop_check_simple_deps_3 = exampleProperty do
  let
    aName = simpleModule "A"
    bName = simpleModule "B"
    cName = simpleModule "C"
    dName = simpleModule "D"
    a = ModuleHeader (aName :~ emptySpan) [] [haskellImport bName]
    b = ModuleHeader (bName :~ emptySpan) [] [haskellImport dName]
    c = ModuleHeader (cName :~ emptySpan) [] [haskellImport bName, haskellImport dName]
    d = ModuleHeader (dName :~ emptySpan) [] []
  dependencyList [a, b, c, d] === Right
    [ Set.singleton dName
    , Set.singleton bName
    , Set.fromList [ aName, cName ]
    ]

prop_check_simple_cycle :: Property
prop_check_simple_cycle = exampleProperty do
  let
    aName = simpleModule "A"
    bName = simpleModule "B"
    cName = simpleModule "C"
    a = ModuleHeader (aName :~ emptySpan) [] [haskellImport bName]
    b = ModuleHeader (bName :~ emptySpan) [] [haskellImport aName, haskellImport cName]
    c = ModuleHeader (cName :~ emptySpan) [] []

  case dependencyList [a, b, c] of
    Left (DependencyCycles cycles) ->
      cycles === Map.fromList [(aName, [bName]), (bName, [aName])]
    Right res -> do
      annotate "Expected failure"
      footnoteShow res
      failure

tests :: IO Bool
tests = checkParallel $$discover
