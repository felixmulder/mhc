module Lexer.Util
  ( charLit
  , intLit
  , strLit
  ) where

import           Prelude
import           Control.Applicative ((<|>), empty)
import           Data.Char (digitToInt, intToDigit)
import           Data.List (foldl', transpose)
import           Numeric (showIntAtBase)
import           Text.Trifecta (Parser)
import           Text.Trifecta (between, char, characterChar, choice, decimal)
import           Text.Trifecta (hexadecimal, highlight, many, notFollowedBy)
import           Text.Trifecta (octal, oneOf, satisfy, skipSome, space, string)
import           Text.Trifecta (try, unexpected, upper)
import           Text.Trifecta ((<?>))
import           Text.Parser.Token.Highlight (Highlight(..))

-- Extra combinators lifted from `parsers` but without stripping whitespace
intLit :: Parser Integer
intLit = sign <*> highlight Number nat
  where
    nat :: Parser Integer
    nat = zeroNumber <|> decimal

    sign :: Parser (Integer -> Integer)
    sign = highlight Operator $
      negate <$ char '-' <|> id <$ char '+' <|> pure id
{-# INLINE intLit #-}

zeroNumber :: Parser Integer
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> pure 0) <?> ""

charLit :: Parser Char
charLit = highlight CharLiteral lit
  where
    lit = between (char '\'') (char '\'' <?> "end of character") characterChar
{-# INLINE charLit #-}

strLit :: Parser String
strLit = highlight StringLiteral lit
  where
    lit :: Parser String
    lit = foldr (maybe id (:)) ""
      <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
      <?> "string"

    stringChar = Just <$> stringLetter <|> stringEscape <?> "string character"

    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape = highlight EscapeCode $ char '\\' *> esc where
      esc = Nothing <$ escapeGap
        <|> Nothing <$ escapeEmpty
        <|> Just <$> escapeCode

    escapeEmpty = char '&'
    escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE strLit #-}

escapeCode :: Parser Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
    charControl :: Parser Char
    charControl =
      (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))

    charNum :: Parser Char
    charNum = toEnum <$> num
      where
        num = bounded (10 :: Int) maxchar
          <|> (char 'o' *> bounded 8 maxchar)
          <|> (char 'x' *> bounded 16 maxchar)

        maxchar = fromEnum (maxBound :: Char)

    charEsc :: Parser Char
    charEsc = choice $ parseEsc <$> escMap

    parseEsc (c,code) = code <$ char c
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
{-# INLINE escapeCode #-}

charAscii :: Parser Char
charAscii = choice $ parseAscii <$> asciiMap
  where
    parseAscii (asc,code) = try $ code <$ string asc

    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes =
      [ "BS","HT","LF","VT","FF","CR","SO"
      , "SI","EM","FS","GS","RS","US","SP"]

    ascii3codes =
      ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
      ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
      ,"SYN","ETB","CAN","SUB","ESC","DEL"]

    ascii2 =
      "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
    ascii3 =
      "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"
{-# INLINE charAscii #-}

-- | Can be used to create parsers for numbers using 'base' and 'upperBound'
bounded :: Int -> Int -> Parser Int
bounded base upperBound = fmap
  (foldl' (\x d -> base * x + digitToInt d) 0)
  (bounded' (take base digits) (digitToInt <$> showIntAtBase base intToDigit upperBound ""))
  where
    digits :: [Parser Char]
    digits = map char ['0'..'9'] ++ map oneOf (transpose [['A'..'F'],['a'..'f']])

    bounded' :: [Parser Char] -> [Int] -> Parser [Char]
    bounded' dps@(zero:_) bds =
      skipSome zero *> ([] <$ notFollowedBy (choice dps) <|> bounded'' dps bds)
                              <|> bounded'' dps bds
    bounded' [] _ = error "bounded called with base 0"

    bounded'' :: [Parser Char] -> [Int] -> Parser [Char]
    bounded'' dps [] =
      [] <$ notFollowedBy (choice dps) <|> toomuch
    bounded'' dps (bd : bds) =
      let
        anyd :: Parser Char
        anyd = choice dps

        nomore :: Parser ()
        nomore = notFollowedBy anyd <|> toomuch

        (low, ex : high) = splitAt bd dps
      in
        ((:) <$> choice low <*> atMost (length bds) anyd) <* nomore
        <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
        <|> if not (null bds) then
              (:) <$> choice high <*> atMost (length bds - 1) anyd <* nomore
            else empty

    atMost n p
      | n <= 0    = pure []
      | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

    toomuch :: Parser a
    toomuch = unexpected "out-of-range numeric escape sequence"
{-# INLINE bounded #-}
