module Source
  ( Module(..)

  , Import

  , Decl

  , ModuleHeader(..)
  ) where

import           Prelude (Show)
import           Data.Text (Text)
import           Data.String (IsString)

data Module = Module
  { header  :: ModuleHeader -- ^ The module name and prefix
  , imports :: [Import]     -- ^ The imports for the module
  , decls   :: [Decl]  -- ^ Statements and declarations
  }
  deriving stock (Show)

data ModuleHeader = ModuleHeader
  { name   :: ModuleName    -- ^ The module's name "Z" in "X.Y.Z"
  , prefix :: [ModuleName]  -- ^ The path to the module "X.Y" in "X.Y.Z"
  }
  deriving stock (Show)

newtype ModuleName = ModuleName Text
  deriving newtype (IsString, Show)

data Import = Import
  deriving stock (Show)

data Decl = Decl
  deriving stock (Show)
