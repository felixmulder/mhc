module Lexer.Types
  ( Token(..)
  , anyTokSpace
  , anyTokUpper
  , anyTokLower
  , tokDot
  , isWhitespace
  ) where

import           Prelude
import           Data.Text (Text)

data Token
  -- Whitespace
  = TokBlockComment Text
  | TokLineComment Text
  | TokSpace Int
  | TokCrlf
  | TokEof
  -- Symbols
  | TokAt
  | TokBacktick
  | TokComma
  | TokHash
  | TokLBrace
  | TokLBracket
  | TokLParen
  | TokPipe
  | TokRBrace
  | TokRBracket
  | TokRParen
  | TokSymChar Char
  | TokTick
  -- Identifiers
  | TokUpperName Text
  | TokLowerName Text
  -- Literals
  | TokCharLit Char
  | TokIntegerLit Integer
  | TokStringLit Text
  -- Keywords
  | TokAs
  | TokClass
  | TokData
  | TokDeriving
  | TokDo
  | TokForall
  | TokImport
  | TokIn
  | TokLet
  | TokModule
  | TokNewtype
  | TokType
  | TokWhere
  deriving stock (Eq, Show)

-----------------------------------------------------------------------------
--
-- Placeholder tokens to illustrate expected tokens
--
-----------------------------------------------------------------------------
anyTokSpace :: Token
anyTokSpace = TokSpace 137

anyTokUpper :: Token
anyTokUpper = TokUpperName "<any>"

anyTokLower :: Token
anyTokLower = TokLowerName "<any>"

tokDot :: Token
tokDot = TokSymChar '.'

isWhitespace :: Token -> Bool
isWhitespace = \case
  TokCrlf    -> True
  TokSpace _ -> True
  _          -> False
