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

  , DependencyCycle(..)
  , dependencyList
  ) where

import           Prelude
import           Control.Comonad (extract)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map (fromList)
import           Data.Set (Set, (\\))
import           Data.String (IsString)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Text.Trifecta (Spanned(..))

-- | Get a dependency ordered list of modules
--
--   Analyze the dependency between modules and figure out in which order to
--   compile things. The produced list should be compiled in order, each entry in
--   the list may be compiled in parallel.
--
--   Will fail if it detects a cycle
dependencyList :: [ModuleHeader] -> Either DependencyCycle [Set ModuleName]
dependencyList headers = go (Set.fromList headers) []
  where
    headersWithDeps :: Map ModuleName [ModuleName]
    headersWithDeps =
      Map.fromList $ headers <&> \h -> (extract . name $ h, dependencies h)

    dependencies :: ModuleHeader -> [ModuleName]
    dependencies (ModuleHeader _ _ xs) =
      xs & fmap \case
        MkHaskellImport (HaskellImport n _ _) :~ _ -> n

    go :: Set ModuleHeader -> [Set ModuleName] -> Either DependencyCycle [Set ModuleName]
    go modules compilationOrder
      -- No more modules to go through, dependency order established:
      | Set.null modules = Right (reverse compilationOrder)
      -- Getting roots was not possible, a cycle must exist
      | Set.null roots = Left . DependencyCycle $ Set.toList modules <&> getCycle
      -- Still building the list, add roots to compilation order and remove
      -- from modules, rinse repeat
      | otherwise = go (modules \\ roots) (moduleNames roots : compilationOrder)
      where
        roots =
          Set.filter (null . localDependencies . extract . name) modules

        moduleNames =
          Set.map (extract . name)

        localDependencies =
          filter (\m -> Set.member m (moduleNames modules)) . (!) headersWithDeps

        -- Find any dependency of 'm' that has a dependency on 'm'
        getCycle m =
          let
            mName = extract (name m)

            dependsOnM = (`elem` headersWithDeps ! mName)

            depsOfM = filter dependsOnM (Set.toList modules <&> extract . name)

            cycles = filter (any (== mName) . (!) headersWithDeps) depsOfM
          in
            (mName, cycles)

newtype DependencyCycle =
  DependencyCycle [(ModuleName, [ModuleName])]

-- | Lower-case identifier
newtype Ident = Ident Text
  deriving newtype (Eq, IsString, Show)

-- | Proper name e.g. type, class, data
newtype ProperName = ProperName Text
  deriving newtype (Eq, IsString, Ord, Show)

data QualifiedName a b = QualifiedName [a] b
  deriving stock (Eq, Show)

instance (Ord a, Ord b) => Ord (QualifiedName a b) where
  compare (QualifiedName xs1 x1) (QualifiedName xs2 x2) =
    let
      xsComp = compare xs1 xs2
      xComp = compare x1 x2
    in
      if xsComp /= EQ then xsComp
      else xComp

newtype ModuleName = ModuleName (QualifiedName ProperName ProperName)
  deriving newtype (Eq, Ord, Show)

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

instance Ord ModuleHeader where
  compare (ModuleHeader h1 _ _) (ModuleHeader h2 _ _) =
    compare h1 h2
