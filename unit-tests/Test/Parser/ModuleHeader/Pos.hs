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

import           Prelude hiding (lex, span)

import           Control.Lens ((^.))
import           Control.Monad (join, filterM)
import           Control.Comonad (extract)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import           Data.Functor ((<&>))
import           Data.List (isPrefixOf)
import qualified Data.List as List (intercalate)
import           Data.String (fromString)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog
import           System.Directory
import           System.Info (os)
import           System.Process (readProcess)
import           Text.Trifecta (Result(..), Span(..), Spanned, span)
import           Text.Trifecta.Delta (Delta(..))

import           Lexer (lex)
import           Lexer.Types (Token)
import           Parser.ModuleHeader (ModuleHeader(..), ModuleName(..), Export(..))
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
    lexFile fp = withFrozenCallStack (name, prop)
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
              footnote $ "From tokens:" <> showTokens tokens
              footnote $ "File contents:\n" <> show fileContents
              failure
        showTokens
          = (\xs -> "\n  [ " <> xs <> " ]")
          . List.intercalate ",\n    "
          . fmap (show . extract)

    -- Reads a file and applies the CPP
    readFileCPP :: FilePath -> IO ByteString
    readFileCPP fp = do
      -- FIXME:
      --   Using `cpp` here for darwin works but is not 1:1 with GHC
      --   Usage of `gcc` here is broken
      let pp = if "darwin" `isPrefixOf` os then "cpp" else "gcc"
      readProcess pp ["-E", "-traditional", "-w", fp] ""
        <&> unlines . drop 7 . lines
        <&> UTF8.fromString

prop_parse_examples :: Property
prop_parse_examples = exampleProperty do
  headerFrom "module Main ( \n ) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === []

  headerFrom "module Main.Party (  foo , ) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName ["Main"] "Party"
    fmap extract exports === [ExportIdent [] "foo"]

  headerFrom "module Main.Party.Central (  foo , ) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName ["Main", "Party"] "Central"
    fmap extract exports === [ExportIdent [] "foo"]

  headerFrom "module Main (  foo , bar ) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === fmap (ExportIdent []) ["foo", "bar"]

  headerFrom "module Main (  foo , bar, ) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === fmap (ExportIdent []) ["foo", "bar"]

  headerFrom "module Main (\n  Foo,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === fmap (ExportIdent []) ["Foo", "Bar"]

  headerFrom "module Main (\n  Foo.bar,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === [ExportIdent ["Foo"] "bar", ExportIdent [] "Bar"]

  headerFrom "module Main (\n  Foo.bar,\n  Bar.Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract moduleName === ModuleName [] "Main"
    fmap extract exports === [ExportIdent ["Foo"] "bar", ExportIdent ["Bar"] "Bar"]

  -- This test makes sure that adding spans together for getting the
  -- appropriate location works as expected
  headerFrom "module Main.Foo (\n  Foo,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    moduleName ^. span === Span (Columns 7 7) (Columns 16 16) "module Main.Foo (\n"

headerFrom :: HasCallStack => ByteString -> PropertyT IO ModuleHeader
headerFrom bs = withFrozenCallStack $ lexThrow bs <&> parseModuleHeader >>= \case
  Right header -> pure header
  Left errs -> do
    footnote . show $ errs
    failure

lexThrow :: ByteString -> PropertyT IO [Spanned Token]
lexThrow e = withFrozenCallStack $ case lex e of
  Success tokens -> pure tokens
  Failure errs -> do
      footnote . show $ errs
      failure

sourceDirs :: [FilePath]
sourceDirs =
  [ "./unit-tests/Test/"
  , "./unit-tests/Test/Parser/"
  , "./unit-tests/Test/Parser/ModuleHeader/"
  , "./unit-tests/Test/Lexer/"
  , "./app/"
  --, "./compiler/"
  --, "./compiler/Lexer/"
  --, "./compiler/Parser/"
  ]
