module Parser.ModuleHeader
  ( Export(..)
  , Import(..)
  , ModuleHeader(..)
  , ModuleName(..)

  , parseModuleHeader
  ) where

import           Prelude hiding (span)
import           Control.Applicative ((<|>), optional, many)
import           Data.Foldable (toList)
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Text.Trifecta (Span, Spanned(..))

import           Lexer.Types (Token(..), tokDot, anyTokSpace, anyTokUpper, anyTokLower)
import           Parser.TreeParser (MonadTreeParser(..), TreeParser, ParserErrors)
import           Parser.TreeParser (ParserError(..))
import           Parser.TreeParser (parseError, runTreeParser, acceptAnySpace)

data ModuleName = ModuleName [Text] Text
  deriving stock (Eq, Show)

instance Semigroup ModuleName where
  ModuleName xs1 n1 <> ModuleName xs2 n2 =
    ModuleName (xs1 <> [n1] <> xs2) n2

data Export
  = ExportIdentifier Text
  | ExportModule Text
  deriving stock (Eq, Show)

data Import = Import
  deriving stock (Eq, Show)

data ModuleHeader = ModuleHeader
  { moduleName :: Spanned ModuleName
  , exports    :: [Spanned Export]
  , imports    :: [Spanned Import]
  , docstring  :: Maybe Docstring
  }
  deriving stock (Eq, Show)

newtype Docstring = Docstring Text
  deriving newtype (Eq, Show)

-- | Parsing a module header from tokens
parseModuleHeader :: [Spanned Token] -> Either ParserErrors ModuleHeader
parseModuleHeader = runTreeParser $ do
  skipWhitespace
  doc <- parseDocstring
  accept TokModule >> skipWhitespace
  ModuleHeader
    <$> parseModuleName
    <*> parseExports
    <*> parseImports
    <*> pure doc

-- | FIXME: currently discards docstring
parseDocstring :: TreeParser (Maybe Docstring)
parseDocstring = peekToken >>= \case
  TokBlockComment _ :~ _ -> skipToken >> skipWhitespace >> parseDocstring
  TokLineComment _  :~ _ -> skipToken >> skipWhitespace >> parseDocstring
  _other                 -> pure Nothing

parseModuleName :: TreeParser (Spanned ModuleName)
parseModuleName = getToken >>= \case
  TokUpperName n :~ span ->
    consumeModuleName span span (Text.pack n) <* skipWhitespace
  other -> parseError (MismatchedToken anyTokUpper other)
  where
    consumeModuleName :: Span -> Span -> Text -> TreeParser (Spanned ModuleName)
    consumeModuleName start end n = do
      (prefix, name) :~ span <- qualifiedIdent start end [] n
      pure $ ModuleName prefix name :~ span

qualifiedIdent :: Span -> Span -> [Text] -> Text -> TreeParser (Spanned ([Text], Text))
qualifiedIdent start end prefix name = peekToken >>= \case
  TokSpace _     :~ span -> (prefix, name) :~ (start <> end <> span) <$ skipToken
  TokCrlf        :~ span -> (prefix, name) :~ (start <> end <> span) <$ skipToken
  TokSymChar '.' :~ span ->
    skipToken >> acceptUpperName \newName newEnd ->
      qualifiedIdent (start <> end <> span) newEnd (prefix ++ [name]) newName
  other ->
    parseError $ ExpectedOtherToken [anyTokSpace, TokCrlf, tokDot] other
  where
    acceptUpperName :: (Text -> Span -> TreeParser a) -> TreeParser a
    acceptUpperName f = getToken >>= \case
      TokUpperName n :~ span -> f (Text.pack n) span
      other -> parseError (MismatchedToken anyTokUpper other)

-- | Parses exports from a module
--
--   Examples:
--
--   @
--   fooBar,
--   FooBar,
--   TODO:
--   Foo.Bar,
--   Foo.bar,
--   A(B, C, D),
--   A.B(A.C, A,D),
--   A.B(..),
--   module A.B,
--   module A.B(X),
--   @
parseExports :: TreeParser [Spanned Export]
parseExports = do
  accept TokLParen >> skipWhitespace
  first <- many (commad <* skipWhitespace)
  lastM <- optional (exported <* skipWhitespace)
  accept TokRParen >> acceptAnySpace >> skipWhitespace
  accept TokWhere >> skipWhitespace
  pure (first ++ toList lastM)

  where
    commad :: TreeParser (Spanned Export)
    commad = exported <* skipWhitespace <* accept TokComma

    exported :: TreeParser (Spanned Export)
    exported = exportedLower
           <|> exportedUpper

    exportedLower :: TreeParser (Spanned Export)
    exportedLower = getToken >>= \case
      TokLowerName n :~ span -> pure $ ExportIdentifier (Text.pack n) :~ span
      other                  -> parseError (MismatchedToken anyTokLower other)

    exportedUpper :: TreeParser (Spanned Export)
    exportedUpper = getToken >>= \case
      TokUpperName n :~ span -> pure $ ExportIdentifier (Text.pack n) :~ span
      other                  -> parseError (MismatchedToken anyTokUpper other)

parseImports :: TreeParser [Spanned Import]
parseImports = pure []
