module Parser.TreeParser
  ( MonadTreeParser(..)
  , TreeParser
  , TreeParserT(..)
  , ParserErrors
  , ParserError(..)
  , runTreeParser
  , parseError
  , try

  , acceptAnySpace
  ) where

import           Prelude hiding (span)
import           Control.Applicative (Alternative)
import           Control.Monad (void)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State (MonadState(..), StateT(..))
import           Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import           Text.Trifecta (Spanned(..))

import           Lexer.Types (Token(..), anyTokSpace)

type ParserErrors = [ParserError]

-- | Parser Error
data ParserError
  = NoTokensLeft
  | ExpectedModuleName (Spanned Token)
  | ExpectedOtherToken [Token] (Spanned Token)
  | MismatchedToken Token (Spanned Token)
  | DebugError String
  deriving stock (Show, Eq)

newtype TreeParserT m a =
  TreeParserT { unTreeParserT :: StateT [Spanned Token] (ExceptT ParserErrors m) a }
  deriving (Applicative, Alternative, Functor, Monad, MonadState [Spanned Token], MonadError ParserErrors)

type TreeParser = TreeParserT Identity

-- | Run tree parser
runTreeParser :: TreeParser a -> [Spanned Token] -> Either ParserErrors a
runTreeParser parser tokens = fst <$> runTreeParser' parser tokens

-- | Run tree parser without discarding new state
runTreeParser' :: TreeParser a -> [Spanned Token] -> Either ParserErrors (a, [Spanned Token])
runTreeParser' parser tokens
  = runIdentity
  . runExceptT
  $ runStateT (unTreeParserT parser) tokens

class Monad m => MonadTreeParser m where
  -- | Gets the current token and pops it from the state
  getToken :: m (Spanned Token)
  -- | Peek at next token, does not modify state
  peekToken :: m (Spanned Token)
  -- | Skip the next token
  skipToken :: m ()
  -- | Accept token, error if token mismatch
  accept :: Token -> m ()
  ---- | Get the current position as a 'Span'
  --getPosition :: m Span
  -- | Skips whitespace until first non-whitespace token
  skipWhitespace :: m ()
  skipWhitespace = peekToken >>= \case
    TokSpace _ :~ _ -> getToken *> skipWhitespace
    TokCrlf :~ _    -> getToken *> skipWhitespace
    _               -> pure ()

instance Monad m => MonadTreeParser (TreeParserT m) where
  getToken = get >>= \case
    t : rest -> put rest >> return t
    [] -> throwError [NoTokensLeft]

  peekToken = get >>= \case
    t : _ -> pure t
    [] -> noTokensLeft

  skipToken = void getToken

  accept expected = get >>= \case
    (token@(actual :~ _) : rest)
      | actual == expected -> put rest
      | otherwise -> parseError $ MismatchedToken expected token
    [] -> noTokensLeft

  --getPosition = get >>= \case
  --  (_ :~ pos) : _ -> pure pos
  --  [] -> noTokensLeft

-- | Runs the tree parser, but resets the parser to the state it was in before
--   attempting to run the parser
--
--   Usage:
--    try a <|> b
try :: TreeParser a -> TreeParser a
try ma = get >>= \tokens ->
  case runTreeParser' ma tokens of
    Left err ->
      put tokens >> throwError err
    Right (a, newTokens) ->
      put newTokens >> pure a

acceptAnySpace :: TreeParser ()
acceptAnySpace = getToken >>= \case
  TokSpace _ :~ _ -> pure ()
  TokCrlf    :~ _ -> pure ()
  other           -> parseError $ MismatchedToken anyTokSpace other

parseError :: MonadError ParserErrors m => ParserError -> m a
parseError = throwError . pure

noTokensLeft :: MonadError ParserErrors m => m a
noTokensLeft = throwError [NoTokensLeft]
