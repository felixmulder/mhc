module Lexer.Types
  ( Token(..)
  ) where

import           Prelude

data Token
  -- Whitespace
  = TokBlockComment String
  | TokLineComment String
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
  | TokUpperName String
  | TokLowerName String
  -- Literals
  | TokCharLit Char
  | TokIntegerLit Integer
  | TokStringLit String
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

