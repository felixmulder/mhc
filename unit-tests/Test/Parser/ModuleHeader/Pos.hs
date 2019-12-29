-----------------------------------------------------------------------------
-- | This module tests for positive parsing of module headers (name, exports,
--   imports)
--
--   It is able to check the local project (i.e. including this source file).
--   As well as being able to parse a corpus of other projects currently
--   including:
--
--   * -
--
--   It also does some example based checking to sanity check the
--   parsing
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Test.Parser.ModuleHeader.Pos
  ( tests
  ) where

import           Prelude hiding (lex)

import           Control.Monad (join, filterM)
import           Control.Comonad (extract)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import           Data.Functor ((<&>))
import           Data.List (isPrefixOf)
import           Data.String (fromString)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog
import           System.Directory
import           System.Info (os)
import           System.Process (readProcess)
import           Text.Trifecta (Result(..), Spanned)

import           Lexer (lex)
import           Lexer.Types (Token)
import           Parser.ModuleHeader (ModuleHeader(..), ModuleName(..))
import           Parser.ModuleHeader (parseModuleHeader)
import           Test.Util (exampleProperty)

tests :: IO Bool
tests = do
  let (Group name props) = $$discover
  fps <- join <$> traverse fileProps sourceDirs
  checkParallel $ Group name (props <> fps)

fileProps :: FilePath -> IO [(PropertyName, Property)]
fileProps dir = do
  filesAndDirs <- fmap (dir <>) <$> listDirectory dir
  fmap lexFile <$> filterM doesFileExist filesAndDirs
  where
    lexFile :: HasCallStack => FilePath -> (PropertyName, Property)
    lexFile fp = withFrozenCallStack $ (name, prop)
      where
        name = fromString ("Parse module header in file " <> fp)
        prop = withFrozenCallStack $ exampleProperty do
          fileContents <- evalIO (readFileCPP fp)
          tokens <- lexThrow fileContents
          case parseModuleHeader tokens of
            Right _ -> pure ()
            Left errs -> do
              footnote $ "Failed to parse module header in file: " <> fp
              footnote . show $ errs
              footnote $ "From tokens:" <> show tokens
              footnote $ "File contents:\n" <> show fileContents
              failure

    -- Reads a file and applies the CPP
    readFileCPP :: FilePath -> IO ByteString
    readFileCPP fp = do
      let pp = if "darwin" `isPrefixOf` os then "cpp" else "gcc"
      readProcess pp ["-E", "-traditional", "-w", fp] ""
        <&> unlines . drop 7 . lines
        <&> UTF8.fromString

lexThrow :: ByteString -> PropertyT IO [Spanned Token]
lexThrow e = withFrozenCallStack $ case lex e of
  Success tokens -> pure tokens
  Failure errs -> do
      footnote . show $ errs
      failure

prop_parse_simplest :: Property
prop_parse_simplest = exampleProperty $ do
  lexThrow "module Main () where" <&> parseModuleHeader >>= \case
    Right ModuleHeader{..} -> do
      extract moduleName === ModuleName [] "Main"
      exports === []
      imports === []
    Left errs -> do
      footnote . show $ errs
      failure

sourceDirs :: [FilePath]
sourceDirs =
  [ "./unit-tests/"
  , "./unit-tests/Test/"
  ]
