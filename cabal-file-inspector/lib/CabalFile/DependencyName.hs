module CabalFile.DependencyName where

import Cabal.Dependency qualified as Cabal (Dependency)
import Cabal.LibraryName qualified
import Cabal.Optics qualified
import CabalFile.CabalOptic
import CabalFile.ComponentName
import CabalFile.PackageName
import CabalFile.Text
import Data.Bool ((||))
import Essentials
import Optics
import Set (Set)
import Set qualified

data DependencyName
  = InternalDependencyName ComponentName
  | ExternalDependencyName PackageName
  deriving stock (Eq, Ord, Show)

data DependencyNameSet = DependencyNameSet
  { internal :: Set ComponentName
  , external :: Set PackageName
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup DependencyNameSet where
  a <> b =
    DependencyNameSet
      { internal = a.internal <> b.internal
      , external = a.external <> b.external
      }

instance Monoid DependencyNameSet where
  mempty = DependencyNameSet mempty mempty

data DependencyType = Internal | External
  deriving stock (Eq, Ord, Enum, Bounded, Show)

class DependencyNameOptic opticKind s | s -> opticKind where
  dependencyName :: Optic' opticKind NoIx s DependencyName

data PackageNamingInfo = PackageNamingInfo
  { packageName :: PackageName
  , internalLibraryNames :: Set ComponentName
  }

dependencyType :: Getter (PackageNamingInfo, Cabal.Dependency) DependencyType
dependencyType = to \(context, dependency) ->
  let name = view (Cabal.Optics.dependency_packageName % re cabal) dependency
  in  ( if name == context.packageName
          || Set.member (name ^. isoViaShortText) context.internalLibraryNames
          then Internal
          else External
      )

instance DependencyNameOptic A_Fold (PackageNamingInfo, Cabal.Dependency) where
  dependencyName = folding \cd@(context, dependency) ->
    case cd ^. dependencyType of
      Internal -> toListOf o dependency
       where
        o =
          Cabal.Optics.dependency_libraryNames
            % Cabal.Optics.nonEmptySet_nonEmpty
            % traversed
            % to \case
              Cabal.LibraryName.LMainLibName -> context.packageName ^. isoViaShortText
              Cabal.LibraryName.LSubLibName x -> x ^. re cabal
            % to InternalDependencyName
      External -> toListOf o dependency
       where
        o =
          Cabal.Optics.dependency_packageName
            % re cabal
            % to ExternalDependencyName
