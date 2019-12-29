-----------------------------------------------------------------------------
-- | This module tests for positive lexing
--
--   It is able to check the local project (i.e. including this source file).
--   As well as being able to lex a corpus of other projects currently
--   including:
--
--   * Lens
--   * Servant
--
--   It also does some example based checking to sanity check the
--   tokenization
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Test.Lexer.Pos
  ( tests
  ) where

import           Prelude hiding (lex)

import           Control.Monad (join, filterM)
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
import           Text.Trifecta (Result(..), Spanned(..))

import           Lexer (lex)
import           Lexer.Types (Token(..))
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
        name = fromString ("Lex file " <> fp)
        prop = withFrozenCallStack $ exampleProperty do
          evalIO (readFileCPP fp <&> lex) >>= \case
            Success _ -> pure ()
            Failure errs -> do
              footnote $ "Failed to lex file: " <> fp
              footnote . show $ errs
              failure

    -- Reads a file and applies the CPP
    readFileCPP :: FilePath -> IO ByteString
    readFileCPP fp = do
      let pp = if "darwin" `isPrefixOf` os then "clang" else "gcc"
      readProcess pp ["-E", "-traditional", "-w", fp] ""
        <&> unlines . drop 8 . lines
        <&> UTF8.fromString

prop_snake_case_lex :: Property
prop_snake_case_lex = exampleProperty do
  do
    tokens <- filterSpace . despan <$> successLex "snake_case_token :: Foo"
    tokens === [ TokLowerName "snake_case_token"
               , TokSymChar ':', TokSymChar ':'
               , TokUpperName "Foo"
               , TokEof
               ]

  do
    tokens <- filterSpace . despan <$> successLex "module Foo.Bar_z () where"
    tokens === [ TokModule
               , TokUpperName "Foo" , TokSymChar '.', TokUpperName "Bar_z"
               , TokLParen, TokRParen
               , TokWhere
               , TokEof
               ]

prop_lex_eol_comment :: Property
prop_lex_eol_comment = exampleProperty do
  do
    tokens <- filterSpace . despan <$> successLex "module Foo.Bar_z --() where"
    tokens === [ TokModule
               , TokUpperName "Foo" , TokSymChar '.', TokUpperName "Bar_z"
               , TokLineComment "--() where"
               , TokEof
               ]
  do
    tokens <- filterSpace . despan <$> successLex "module Foo.Bar_z--() where"
    tokens === [ TokModule
               , TokUpperName "Foo" , TokSymChar '.', TokUpperName "Bar_z"
               , TokLineComment "--() where"
               , TokEof
               ]


successLex :: HasCallStack => ByteString -> PropertyT IO [Spanned Token]
successLex bs = withFrozenCallStack do
  case lex bs of
    Success tokens -> pure tokens
    Failure errs -> do
      footnote . show $ errs
      failure

filterSpace :: [Token] -> [Token]
filterSpace = join . fmap go
  where
    go (TokSpace _) = []
    go other        = [other]

despan :: [Spanned a] -> [a]
despan = fmap go
  where go (a :~ _) = a

-- | All source directories that are being lexed in this project currently
sourceDirs :: [FilePath]
sourceDirs =
  localProject ++
  lens ++
  servant ++
  []

localProject :: [FilePath]
localProject =
  [ "./unit-tests/lexer/pos/"
  , "./unit-tests/Test/"
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
  [ "Control/"
  , "Control/Exception/"
  , "Control/Lens/"
  , "Control/Lens/Internal/"
  , "Control/Monad/", "Control/Monad/Error/"
  , "Control/Parallel/", "Control/Parallel/Strategies/"
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
  , "Data/Text/", "Data/Text/Lazy/", "Data/Text/Strict/"
  , "Data/Tree/"
  , "Data/Typeable/"
  , "Data/Vector/"
  , "Data/Vector/Generic/"
  , "GHC/"
  , "GHC/Generics/"
  , "Language/", "Language/Haskell/", "Language/Haskell/TH/"
  , "Numeric/", "Numeric/Natural/"
  , "System/", "System/Exit/", "System/FilePath/", "System/IO/", "System/IO/Error/"
  ]

servant :: [FilePath]
servant = fmap ("./corpus/servant/servant/src/" <>) $
  [ "Servant/"
  , "Servant/API/", "Servant/API/Experimental/", "Servant/API/Internal/"
  , "Servant/API/Internal/Test/"
  , "Servant/Test/"
  , "Servant/Types/"
  , "Servant/Utils/"
  ]
