module Such.Lexing
  ( foo
  , bar
  , baz
  , Foo
  , Bar(Bar)
  , Baz(..)
  ) where

foo :: Int -> String -> Balloon
foo = return (1 + 2) >> toBalloon

balloon_jump :: Int -> String -> IO ()
balloon_jump = undefined

snake_case :: Int
