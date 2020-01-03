module Parser.ModuleHeader
  ( Import(..)
  , ModuleExport(..)
  , ModuleHeader(..)
  , ModuleName(..)
  , QualifiedName(..)
  , DataMembers(..)

  , parseModuleHeader
  ) where

import           Prelude hiding (span)
import           Control.Applicative ((<|>), optional, many)
import           Control.Comonad (extract)
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.String (IsString)
import           Data.Text (Text, pack)
import           Text.Trifecta (Span, Spanned(..))

import           Lexer.Types (Token(..))
import           Lexer.Types (tokDot, anyTokSpace, anyTokUpper, anyTokLower)
import           Parser.TreeParser (MonadTreeParser(..), TreeParser, ParserErrors)
import           Parser.TreeParser (ParserError(..))
import           Parser.TreeParser (parseError, runTreeParser, acceptAnySpace)

-- | Lower-case identifier
newtype Ident = Ident Text
  deriving newtype (Eq, IsString, Show)

-- | Proper name e.g. type, class, data
newtype ProperName = ProperName Text
  deriving newtype (Eq, IsString, Show)

data QualifiedName a b = QualifiedName [a] b
  deriving stock (Eq, Show)

newtype ModuleName = ModuleName (QualifiedName ProperName ProperName)
  deriving newtype (Eq, Show)

data ModuleExport
  = MkIdent Ident
  | MkSymbol Ident
  | MkProperName ProperName
  | MkDataExport ProperName DataMembers
  deriving stock (Eq, Show)

data DataMembers
  = OnlyType -- ''
  | Wildcard -- '(..)'
  | Members (NonEmpty ProperName)
  deriving stock (Eq, Show)

data Import = Import
  deriving stock (Eq, Show)

data ModuleHeader = ModuleHeader
  { moduleName :: Spanned ModuleName
  , exports    :: [Spanned ModuleExport]
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
    consumeModuleName span span n <* skipWhitespace
  other -> parseError (MismatchedToken anyTokUpper other)
  where
    consumeModuleName :: Span -> Span -> Text -> TreeParser (Spanned ModuleName)
    consumeModuleName start end n = do
      (prefix, name) :~ span <- qualifiedIdent start end [] n
      pure $
        ModuleName (QualifiedName (ProperName <$> prefix) (ProperName name)) :~ span

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
      TokUpperName n :~ span -> f n span
      other -> parseError (MismatchedToken anyTokUpper other)

-- | Parses exports from a module
--
--   Examples:
--
--   @
--   fooBar,
--   FooBar,
--   A.B(),
--   A(B, C, D),
--   A.B(..),
--   TODO:
--   A.B(A.C, A,D),
--   Foo.bar,
--   Foo.Bar,
--   module A.B,
--   module A.B(X),
--   @
parseExports :: TreeParser [Spanned ModuleExport]
parseExports = do
  accept TokLParen >> skipWhitespace
  first <- many (commad <* skipWhitespace)
  lastM <- optional (exported <* skipWhitespace)
  accept TokRParen >> acceptAnySpace >> skipWhitespace
  accept TokWhere >> skipWhitespace
  pure (first ++ toList lastM)

  where
    commad :: TreeParser (Spanned ModuleExport)
    commad = exported <* skipWhitespace <* accept TokComma

    exported :: TreeParser (Spanned ModuleExport)
    exported = exportedLower
           <|> exportedData
           <|> exportedSymbol
           <|> fmap (mapValue MkProperName) exportedUpper

    mapValue :: (a -> b) -> Spanned a -> Spanned b
    mapValue f (a :~ span) = f a :~ span

    exportedSymbol :: TreeParser (Spanned ModuleExport)
    exportedSymbol = do
      let
        acceptSymchar = getToken >>= \case
          TokSymChar c :~ span -> pure $ c :~ span
          TokPipe      :~ span -> pure $ '|' :~ span
          other                -> parseError $ ExpectedSymchar other

      start <- accept TokLParen
      syms  <- many acceptSymchar
      end   <- accept TokRParen

      let
        spanToSpan (Just acc) s = Just (acc <> s)
        spanToSpan Nothing _ = Nothing
        spanRes =
          case fmap (\(_ :~ span) -> span) syms of
            (x : xs) -> foldl' spanToSpan (Just x) xs
            []       -> Nothing

      case (fmap extract syms, spanRes) of
        (x : xs, Just symSpan) -> pure $
          MkSymbol (Ident $ pack (x : xs)) :~ (start <> symSpan <> end)
        (_, _) -> parseError $
          DebugError "Symbol couldn't be parsed from export list"

    exportedLower :: TreeParser (Spanned ModuleExport)
    exportedLower = peekToken >>= \case
      TokLowerName n :~ span ->
        MkIdent (Ident n) :~ span <$ skipToken
      other                  ->
        parseError (MismatchedToken anyTokLower other)

    exportedUpper :: TreeParser (Spanned ProperName)
    exportedUpper = getToken >>= \case
      TokUpperName n :~ span -> pure $ ProperName n :~ span
      other                  ->
        parseError (MismatchedToken anyTokUpper other)

    exportedData :: TreeParser (Spanned ModuleExport)
    exportedData = do
      name :~ nameSpan <- exportedUpper
      members :~ memSpan <- exportedMembers
      pure $ MkDataExport name members :~ (nameSpan <> memSpan)

    exportedMembers :: TreeParser (Spanned DataMembers)
    exportedMembers = typeOnly
                  <|> wildcard
                  <|> constructors

    typeOnly = do
      start <- accept TokLParen <* skipWhitespace
      end   <- accept TokRParen
      pure $ OnlyType :~ (start <> end)

    wildcard = do
      start <- accept TokLParen
      dot1  <- accept $ TokSymChar '.'
      dot2  <- accept $ TokSymChar '.'
      end   <- accept TokRParen
      pure $ Wildcard :~ (start <> dot1 <> dot2 <> end)

    constructors = do
      start <- accept TokLParen
      first <- many (exportedUpper <* accept TokComma <* skipWhitespace)
      lastM <- optional (exportedUpper <* skipWhitespace)
      end   <- accept TokRParen
      let
        spanToSpan (Just acc) s = Just (acc <> s)
        spanToSpan Nothing _ = Nothing
        spanRes =
          case fmap (\(_ :~ span) -> span) first of
            (x : xs) -> foldl' spanToSpan (Just x) xs
            []       -> Nothing

      case (fmap extract $ first <> toList lastM, spanRes) of
        (x : xs, Just memberSpans) -> pure $ Members (x :| xs) :~ (start <> memberSpans <> end)
        (_, _) -> parseError $ DebugError "Members couldn't be parsed from export list"

parseImports :: TreeParser [Spanned Import]
parseImports = pure []
