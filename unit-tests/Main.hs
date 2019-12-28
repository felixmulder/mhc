module Main
  ( main
  ) where

import           Prelude
import           Hedgehog.Main (defaultMain)

import qualified Test.Lexer.Pos (tests)

main :: IO ()
main = defaultMain
  [ Test.Lexer.Pos.tests
  ]
