module Parser.ModuleHeader
  ( Export(..)
  , Import(..)
  , ModuleHeader(..)
  , ModuleName(..)
  , ParserErrors(..)
  , ParserError(..)

  , parseModuleHeader
  ) where

import           Prelude hiding (span)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State (MonadState(..), StateT, evalStateT)
import           Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import           Data.Function ((&))
import           Data.Text (Text)
import qualified Data.Text as Text (pack, splitOn)
import           Text.Trifecta (Spanned(..))

import           Lexer.Types (Token(..))

data ModuleName = ModuleName [Text] Text
  deriving stock (Eq, Show)

data Export
  = ExportIdentifier Text
  | ExportModule Text
  deriving stock (Eq, Show)

data Import = Import
  deriving stock (Eq, Show)

data ModuleHeader = ModuleHeader
  { moduleName :: Spanned ModuleName
  , exports :: [Spanned Export]
  , imports :: [Spanned Import]
  }
  deriving stock (Eq, Show)

-- | Non-Empty list of parser errors
data ParserErrors = ParserErrors ParserError [ParserError]
  deriving stock (Show, Eq)

-- | Parser Error
data ParserError
  = NoTokensLeft
  | ExpectedModuleName (Spanned Token)
  | MismatchedToken Token (Spanned Token)
  deriving stock (Show, Eq)

newtype TreeParserT m a =
  TreeParserT { unTreeParserT :: StateT [Spanned Token] (ExceptT ParserErrors m) a }
  deriving (Applicative, Functor, Monad, MonadState [Spanned Token], MonadError ParserErrors)

type TreeParser = TreeParserT Identity

class Monad m => MonadTreeParser m where
  -- | Gets the current token and pops it from the state
  getToken :: m (Spanned Token)
  -- | Peek at next token, does not modify state
  peekToken :: m (Spanned Token)
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
    [] -> throwError (ParserErrors NoTokensLeft [])

  peekToken = get >>= \case
    t : _ -> pure t
    [] -> noTokensLeft

  accept expected = get >>= \case
    actual :~ _ : rest
      | actual == expected -> put rest
      | otherwise -> noTokensLeft
    [] -> noTokensLeft

  --getPosition = get >>= \case
  --  (_ :~ pos) : _ -> pure pos
  --  [] -> noTokensLeft

parseError :: MonadError ParserErrors m => ParserError -> m a
parseError = throwError . flip ParserErrors []

noTokensLeft :: MonadError ParserErrors m => m a
noTokensLeft = throwError $ ParserErrors NoTokensLeft []

runTreeParser :: TreeParser a -> [Spanned Token] -> Either ParserErrors a
runTreeParser parser tokens
  = runIdentity
  . runExceptT
  $ evalStateT (unTreeParserT parser) tokens

parseModuleHeader :: [Spanned Token] -> Either ParserErrors ModuleHeader
parseModuleHeader = runTreeParser parseModuleHeader'

-- | Parsing a module header from tokens
--
--   TODO:
--   - Preceding comments
--   - Export list
--   - Import list
parseModuleHeader' :: TreeParser ModuleHeader
parseModuleHeader' = do
  skipWhitespace >> accept TokModule >> skipWhitespace
  name <- expectModuleName
  pure $ ModuleHeader name [] []

  where
    toModuleName :: String -> ModuleName
    toModuleName s = Text.pack s & Text.splitOn "." & listToModuleName []

    listToModuleName prefix (name : []) = ModuleName (reverse prefix) name
    listToModuleName prefix (name : rest) = listToModuleName (name : prefix) rest
    listToModuleName _prefix [] = error "BUG! Empty module name made it through lexing"

    expectModuleName :: TreeParser (Spanned ModuleName)
    expectModuleName = getToken >>= \case
      TokUpperName n :~ span -> pure $ toModuleName n :~ span
      other                  -> parseError (ExpectedModuleName other)

