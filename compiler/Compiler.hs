module Compiler
  ( main
  ) where

import           Prelude
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.ByteString as BS (readFile)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

import qualified Lexer (lex)
import           Text.Trifecta (Result(..))

main :: IO ()
main =
  getArgs >>= \[filePath] -> compileFile filePath

compileFile :: FilePath -> IO ()
compileFile fp = do
  fileContents <- liftIO (BS.readFile fp)
  case Lexer.lex fileContents of
    Success tokens ->
      print tokens
    Failure e ->
      print e >> exitFailure
