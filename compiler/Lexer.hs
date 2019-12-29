module Lexer
  ( lex
  ) where

import           Prelude hiding (lex)
import           Control.Applicative ((<|>))
import           Control.Comonad (extract)
import           Control.Monad (join)
import           Data.ByteString (ByteString)
import           Data.Foldable (toList)
import           Data.Functor ((<&>))
import           Text.Trifecta hiding (token, spaces)

import           Lexer.Util (charLit, strLit, intLit)
import           Lexer.Types (Token(..))

lex :: ByteString -> Result [Spanned Token]
lex = parseByteString lexTokens mempty

lexTokens :: Parser [Spanned Token]
lexTokens = do
  s    <- spaces
  c    <- optional comment
  toks <- maybe [] join <$> optional (many tokens)
  end  <- try crlf <|> spanned (TokEof <$ eof)
  let
    currentLine =
      s ++ toList c ++ toks ++ [end]
  if extract end == TokCrlf then
    lexTokens <&> (currentLine ++)
  else
    pure currentLine

tokens :: Parser [Spanned Token]
tokens = do
  t  <- spanned token
  s0 <- spaces
  c  <- optional comment
  s1 <- spaces
  pure $ t : (s0 ++ toList c ++ s1)
  where
    token :: Parser Token
    token = try literal
        <|> try simpleSymbol
        <|> try keyword
        <|> try upperCasedName
        <|> try lowerCasedName

upperCasedName :: Parser Token
upperCasedName = do
  c <- upper
  rest <- many (alphaNum <|> char '_')
  pure $ TokUpperName (c : rest)

lowerCasedName :: Parser Token
lowerCasedName = do
  c    <- choice [char '_', lower]
  rest <- many (alphaNum <|> char '_' <|> char '\'' <|> char '#')
  pure $ TokLowerName (c : rest)


simpleSymbol :: Parser Token
simpleSymbol = choice
  [ TokAt       <$  char '@'
  , TokBacktick <$  char '`'
  , TokComma    <$  char ','
  , TokLBrace   <$  char '{'
  , TokLBracket <$  char '['
  , TokLParen   <$  char '('
  , TokPipe     <$  char '|'
  , TokRBrace   <$  char '}'
  , TokRBracket <$  char ']'
  , TokRParen   <$  char ')'
  , TokTick     <$  char '\''
  , TokHash     <$  char '#'
  , TokSymChar  <$> oneOf "^&*$/+-<>=.:~\\?!%"
  ] <?> "operator"

literal :: Parser Token
literal = choice
  [ TokCharLit    <$> charLit
  , TokStringLit  <$> strLit
  , TokIntegerLit <$> intLit
  ] <?> "literal"

keyword :: Parser Token
keyword = choice
  [ TokAs        <$ string "as"
  , TokData      <$ string "data"
  , TokClass     <$ string "class"
  , TokDeriving  <$ string "deriving"
  , TokDo        <$ string "do"
  , TokForall    <$ string "forall"
  , TokImport    <$ string "import"
  , TokIn        <$ string "in"
  , TokLet       <$ string "let"
  , TokModule    <$ string "module"
  , TokNewtype   <$ string "newtype"
  , TokType      <$ string "type"
  , TokWhere     <$ string "where"
  ] <* notFollowedBy alphaNum <?> "keyword"

crlf :: Parser (Spanned Token)
crlf = spanned $ TokCrlf <$ oneOf "\n\r" <?> "newline"

spaces :: Parser [Spanned Token]
spaces = do
  spcs :~ s <- spanned $ many $ char ' '
  pure
    if null spcs then []
    else [TokSpace (length spcs) :~ s]

comment :: Parser (Spanned Token)
comment =
  spanned (try lineComment <|> blockComment) <?> "comment"

lineComment :: Parser Token
lineComment = do
  contents <- string "--" >> many (noneOf "\r\n")
  pure $ TokLineComment $ "--" ++ contents

blockComment :: Parser Token
blockComment = do
  contents <- string "{-" >> anyChar `manyTill` commentEnd
  pure $ TokBlockComment $ reconstructed $ contents
  where
    commentEnd = try $ string "-}"
    reconstructed c = "{-" ++ c ++ "-}"
