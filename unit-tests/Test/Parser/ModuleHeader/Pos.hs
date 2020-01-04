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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as List (intercalate)
import           Data.String (fromString)
import qualified Data.Text as Text (intercalate)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import           Hedgehog
import           System.Directory
import           System.Info (os)
import           System.Process (readProcess)
import           Text.Trifecta (Result(..), Span(..), Spanned(..), span)
import           Text.Trifecta.Delta (Delta(..))

import           Lexer (lex, discardComments)
import           Lexer.Types (Token)
import           Parser.ModuleHeader.AST (ModuleHeader(..), ModuleName(..), ModuleExport(..))
import           Parser.ModuleHeader.AST (DataMembers(..), QualifiedName(..))
import           Parser.ModuleHeader.AST (Import(..), HaskellImport(..))
import           Parser.ModuleHeader (parseModuleHeader, parseImport)
import           Parser.TreeParser (TreeParser, runTreeParser)
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
          tokens <- discardComments <$> lexThrow fileContents
          case runTreeParser parseModuleHeader tokens of
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
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === []

  headerFrom "module Main.Party (  foo , ) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName ["Main"] "Party")
    fmap extract exports === [MkIdent "foo"]

  headerFrom "module Main.Party.Central (  foo , ) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName ["Main", "Party"] "Central")
    fmap extract exports === [MkIdent "foo"]

  headerFrom "module Main (  foo , bar ) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === fmap MkIdent ["foo", "bar"]

  headerFrom "module Main (  foo , bar, ) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === fmap MkIdent ["foo", "bar"]

  headerFrom "module Main (  (<>), (<~), ) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === fmap MkSymbol ["<>", "<~"]

  headerFrom "module Main (\n  Foo,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === fmap MkProperName ["Foo", "Bar"]

  headerFrom "module Main (\n  Foo() ,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === [MkDataExport "Foo" OnlyType, MkProperName "Bar"]

  headerFrom "module Main (\n  Foo( ) ,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === [MkDataExport "Foo" OnlyType, MkProperName "Bar"]

  headerFrom "module Main (\n  Foo(..) ,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === [MkDataExport "Foo" Wildcard, MkProperName "Bar"]

  headerFrom "module Main (\n  Foo(Foo, Bar, Baz) ,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === [ MkDataExport "Foo" (Members $ "Foo" :| ["Bar", "Baz"])
                             , MkProperName "Bar"
                             ]

  importFrom "import Foo.Bar ()" >>= \case
    MkHaskellImport _hsImport :~ _ -> pure ()

  importFrom "import Foo.Bar (Bar)" >>= \case
    MkHaskellImport (HaskellImport _ xs _) :~ _ ->
      xs === [ MkProperName "Bar" ]

  importFrom "import Foo.Bar (bar)" >>= \case
    MkHaskellImport (HaskellImport _ xs _) :~ _ ->
      xs === [ MkIdent "bar" ]

  let
    importExample = Text.encodeUtf8 $ Text.intercalate "\n" $
      [ "module Main ("
      , "  Foo(Foo, Bar, Baz),"
      , ") where"
      , ""
      , "import Foo.Bar ()"
      ]
  headerFrom importExample >>= \ModuleHeader{..} -> do
    extract name === ModuleName (QualifiedName [] "Main")
    fmap extract exports === [ MkDataExport "Foo" (Members $ "Foo" :| ["Bar", "Baz"]) ]
    let hsImport = HaskellImport (ModuleName $ QualifiedName ["Foo"] "Bar") [] Nothing
    fmap extract imports === [ MkHaskellImport hsImport ]

  -- TODO: Let's forget about re-exports for now
  --headerFrom "module Main (\n  Foo.bar,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
  --  extract name === ModuleName [] "Main"
  --  fmap extract exports === [MkIdent ["Foo"] "bar", MkIdent "Bar"]

  -- TODO: Let's forget about re-exports for now
  --headerFrom "module Main (\n  Foo.bar,\n  Bar.Bar,\n) where" >>= \ModuleHeader{..} -> do
  --  extract name === ModuleName [] "Main"
  --  fmap extract exports === [ExportIdent ["Foo"] "bar", ExportIdent ["Bar"] "Bar"]

  -- This test makes sure that adding spans together for getting the
  -- appropriate location works as expected

  headerFrom "module Main.Foo (\n  Foo,\n  Bar,\n) where" >>= \ModuleHeader{..} -> do
    name ^. span === Span (Columns 7 7) (Columns 16 16) "module Main.Foo (\n"

headerFrom :: HasCallStack => ByteString -> PropertyT IO ModuleHeader
headerFrom = withFrozenCallStack $ parsedFrom parseModuleHeader

lexThrow :: ByteString -> PropertyT IO [Spanned Token]
lexThrow e = withFrozenCallStack $ case lex e of
  Success tokens -> pure tokens
  Failure errs -> do
      footnote . show $ errs
      failure

parsedFrom :: HasCallStack => TreeParser a -> ByteString -> PropertyT IO a
parsedFrom parser bs = withFrozenCallStack $ do
  res <- lexThrow bs
  case runTreeParser parser res of
    Right a -> pure a
    Left errs -> do
      footnote . show $ errs
      failure

importFrom :: HasCallStack => ByteString -> PropertyT IO (Spanned Import)
importFrom = withFrozenCallStack $ parsedFrom parseImport

sourceDirs :: [FilePath]
sourceDirs =
  localProject ++
  lens ++
  servant ++
  []

localProject :: [FilePath]
localProject =
  [ "./unit-tests/Test/"
  , "./unit-tests/Test/Parser/"
  , "./unit-tests/Test/Parser/ModuleHeader/"
  , "./unit-tests/Test/Lexer/"
  , "./app/"
  , "./compiler/"
  , "./compiler/Lexer/"
  , "./compiler/Parser/"
  ]

lens :: [FilePath]
lens = fmap ("./corpus/lens/src/" <>) $
  [ "Control/Parallel/", "Control/Parallel/Strategies/"
  , "Control/Seq/"
  , "Data/"
  , "Data/Array/"
  , "Data/Bits/"
  , "Data/ByteString/", "Data/ByteString/Lazy/", "Data/ByteString/Strict/"
  , "Data/Complex/"
  , "Data/Data/"
  , "Data/Dynamic/"
  , "Data/HashSet/"
  , "Data/IntSet/"
  , "Data/List/"
  , "Data/Map/"
  , "Data/Sequence/"
  , "Data/Set/"
  , "Data/Text/"
  , "Data/Tree/"
  , "Data/Typeable/"
  , "Data/Vector/"
  , "GHC/"
  , "Language/"
  , "Language/Haskell/"
  , "Language/Haskell/TH/"
  , "Numeric/", "Numeric/Natural/"
  , "System/", "System/Exit/", "System/IO/"
  , "System/FilePath/"


  -- FIXME: re-exports
  --, "Control/"

  -- FIXME: System.IO.Error has no explicit exports
  --, "System/IO/Error/"

  -- FIXME: these files have CPP issues:
  --, "Control/Exception/"
  --, "Control/Lens/"
  --, "Control/Lens/Internal/"
  --, "Control/Monad/", "Control/Monad/Error/"
  --, "Data/Text/Lazy/"
  --, "Data/Text/Strict/"
  --, "Data/Vector/Generic/"
  --, "GHC/Generics/"
  ]

servant :: [FilePath]
servant = fmap ("./corpus/servant/servant/src/" <>) $
  [ "Servant/API/Internal/"

  -- FIXME: re-exports:
  --, "Servant/API/Internal/Test/"
  --,"Servant/"
  --, "Servant/Utils/"

  -- FIXME: no explicit exports:
  --, "Servant/API/Experimental/"
  --, "Servant/Test/"
  --, "Servant/Types/"

  -- FIXME: CPP issue
  --, "Servant/API/"
  ]
