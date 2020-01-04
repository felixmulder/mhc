module Parser.ModuleHeader.AST
  ( DataMembers(..)
  , HaskellImport(..)
  , Ident(..)
  , Import(..)
  , ModuleExport(..)
  , ModuleHeader(..)
  , ModuleName(..)
  , ProperName(..)
  , QualifiedName(..)
  ) where

import           Prelude
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.String (IsString)
import           Data.Text (Text)
import           Text.Trifecta (Spanned)

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

data Import
  = MkHaskellImport HaskellImport
  deriving stock (Eq, Show)

data HaskellImport
  = HaskellImport ModuleName [ModuleExport] (Maybe ModuleName)
  deriving stock (Eq, Show)

data ModuleHeader = ModuleHeader
  { name      :: Spanned ModuleName
  , exports   :: [Spanned ModuleExport]
  , imports   :: [Spanned Import]
  }
  deriving stock (Eq, Show)

