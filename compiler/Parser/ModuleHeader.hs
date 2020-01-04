module Parser.ModuleHeader
  ( parseModuleHeader
  , parseImport
  ) where

import           Prelude hiding (span)
import           Control.Applicative ((<|>), optional, many)
import           Control.Comonad (extract)
import           Data.Foldable (toList)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text, pack)
import           Text.Trifecta (Span, Spanned(..))

import           Lexer.Types (Token(..))
import           Lexer.Types (tokDot, anyTokSpace, anyTokUpper, anyTokLower)
import           Parser.TreeParser (MonadTreeParser(..), ParserError(..), TreeParser)
import           Parser.TreeParser (parseError)
import           Parser.ModuleHeader.AST (ModuleHeader(..), ModuleName(..), ModuleExport(..))
import           Parser.ModuleHeader.AST (Ident(..), ProperName(..), DataMembers(..), Import(..))
import           Parser.ModuleHeader.AST (HaskellImport(..), QualifiedName(..))

-- | Parsing a module header from tokens
parseModuleHeader :: TreeParser ModuleHeader
parseModuleHeader = do
  skipWhitespace >> accept TokModule >> skipWhitespace
  ModuleHeader
    <$> parseModuleName
    <*> (parseExports <* accept TokWhere)
    <*> many parseImport

parseModuleName :: TreeParser (Spanned ModuleName)
parseModuleName = getToken >>= \case
  TokUpperName n :~ span ->
    consumeModuleName span span n <* skipWhitespace
  other -> parseError (MismatchedToken anyTokUpper other)
  where
    consumeModuleName :: Span -> Span -> Text -> TreeParser (Spanned ModuleName)
    consumeModuleName start end n = do
      (prefix, moduleName) :~ span <- qualifiedIdent start end [] n
      pure $
        ModuleName (QualifiedName (ProperName <$> prefix) (ProperName moduleName)) :~ span

qualifiedIdent :: Span -> Span -> [Text] -> Text -> TreeParser (Spanned ([Text], Text))
qualifiedIdent start end prefix identName = peekToken >>= \case
  TokSpace _     :~ span -> (prefix, identName) :~ (start <> end <> span) <$ skipToken
  TokCrlf        :~ span -> (prefix, identName) :~ (start <> end <> span) <$ skipToken
  TokEof         :~ span -> pure $ (prefix, identName) :~ (start <> end <> span)
  TokSymChar '.' :~ span ->
    skipToken >> acceptUpperName \newName newEnd ->
      qualifiedIdent (start <> end <> span) newEnd (prefix ++ [identName]) newName
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
  accept TokRParen >> skipWhitespace
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
      dataName :~ nameSpan <- exportedUpper
      members  :~ memSpan  <- exportedMembers
      pure $ MkDataExport dataName members :~ (nameSpan <> memSpan)

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

-- HaskellImport ModuleName [ModuleExport] (Maybe ModuleName)
parseImport :: TreeParser (Spanned Import)
parseImport = haskellImport

haskellImport :: TreeParser (Spanned Import)
haskellImport = do
  skipWhitespace
  start      <- accept TokImport <* skipWhitespace
  qualSpanM  <- optional (accept TokQualified <* skipWhitespace)
  spannedMod <- parseModuleName
  exps       <- parseExports
  asM        <- optional (accept TokAs <* skipWhitespace)
  aliasM     <- optional parseModuleName

  let
    moduleName :~ moduleSpan = spannedMod
    imported = fmap extract exps
    hsImport exs alias = MkHaskellImport (HaskellImport moduleName exs alias)

  case (qualSpanM, asM, aliasM, sequenceSpan exps) of
    (Nothing, Nothing, Nothing, Just (_ :~ exportSpan)) ->
      pure $ hsImport imported Nothing :~ (start <> moduleSpan <> exportSpan)
    (Nothing, Nothing, Nothing, Nothing) ->
      pure $ hsImport [] Nothing :~ (start <> moduleSpan)
    (Just qualSpan, Just asSpan, Just (alias :~ aliasSpan), Just (_ :~ exportSpan)) ->
      pure $ hsImport imported (Just alias) :~ (start <> qualSpan <> asSpan <> aliasSpan <> exportSpan)
    (Just qualSpan, Just asSpan, Just (alias :~ aliasSpan), Nothing) ->
      pure $ hsImport imported (Just alias) :~ (start <> qualSpan <> asSpan <> aliasSpan)
    (Just qualSpan, Nothing, Nothing, Just (_ :~ exportSpan)) ->
      pure $ hsImport imported (Just moduleName) :~ (start <> qualSpan <> exportSpan)
    (Just qualSpan, Nothing, Nothing, Nothing) ->
      pure $ hsImport imported (Just moduleName) :~ (start <> qualSpan)
    (Nothing, Nothing, Just (_ :~ aliasSpan), _) ->
      parseError $ UnexpectedAlias aliasSpan
    (Just _, Nothing, Just (_ :~ aliasSpan), _) ->
      parseError $ UnexpectedAlias aliasSpan
    (Nothing, Just _, Just _, _) ->
      parseError $ MissingQualified moduleSpan
    (Nothing, Just _, Nothing, _) ->
      parseError $ MissingQualified moduleSpan
    (Just _, Just span, Nothing, _) ->
      parseError $ MissingAlias span

sequenceSpan :: [Spanned a] -> Maybe (Spanned [a])
sequenceSpan [] = Nothing
sequenceSpan (x :~ xSpan : xs) =
  Just $ elems :~ spans
  where
    elems = x : fmap extract xs
    spans = foldl' (\acc (_ :~ span) -> acc <> span) xSpan xs
