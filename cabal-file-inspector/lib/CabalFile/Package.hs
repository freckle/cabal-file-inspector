module CabalFile.Package where

import Cabal.Optics qualified
import Cabal.PackageDescription qualified
import Cabal.PackageDescription qualified as Cabal (PackageDescription)
import CabalFile.CabalOptic
import CabalFile.Component
import CabalFile.ComponentName
import CabalFile.ComponentType
import CabalFile.ComponentsByType (ComponentsByType)
import CabalFile.ComponentsByType qualified as ComponentsByType
import CabalFile.DependencyName
import CabalFile.PackageName
import CabalFile.QualifiedComponentName
import CabalFile.Text
import Control.Monad.ST
import Data.Foldable (foldMap)
import Data.STRef
import Essentials
import MonadicFlow (unless, whileJust)
import Optics
import Set (Set)
import Set qualified

data Package = Package
  { name :: PackageName
  , componentsByType :: ComponentsByType
  }

instance
  CabalOptic
    A_Review
    Package
    Package
    Cabal.PackageDescription
    Cabal.PackageDescription
  where
  cabal = unto \x ->
    let
      name = x ^. Cabal.Optics.packageDescription_name % re cabal
      componentsByType =
        ComponentsByType.fromList $
          (x & Cabal.PackageDescription.pkgBuildableComponents) <&> makeComponent name
    in
      Package {name, componentsByType}

instance HasPackageName Package where
  packageName = (.name)

-- | The names of all a package's components of a particular type
--
-- If the type if 'Library', this includes the main library (if the package has a main
-- main library, its component name is implicitly the same as the package name) as well
-- as any sublibraries.
componentsOfType :: ComponentType -> Package -> Set ComponentName
componentsOfType t =
  (.componentsByType)
    >>> ComponentsByType.ofType t
    >>> fmap (.id.name)
    >>> Set.fromList

allComponents :: Package -> Set QualifiedComponentName
allComponents p = Set.fromList (ComponentsByType.toList p.componentsByType <&> (.id))

instance ComponentOptic A_Fold Package where
  component = folding $ (.componentsByType) >>> ComponentsByType.toList

instance DependencyNameOptic A_Fold Package where
  dependencyName = folding $ \p ->
    p & toListOf (component % to (packageNamingContext p,) % dependencyName)

packageNamingContext :: Package -> PackageNamingInfo
packageNamingContext p =
  PackageNamingInfo
    { packageName = p.name
    , internalLibraryNames = componentsOfType Library p
    }

instance DependencyNameOptic A_Fold (Package, QualifiedComponentName) where
  dependencyName =
    folding \(p, i) ->
      ComponentsByType.lookup i p.componentsByType & foldMap \c ->
        toListOf dependencyName (packageNamingContext p, c)

-- | The component name representing a package's main library
--
-- Note that the package might not actually have a main library.
-- This function merely constructs the name that the component would have if it exists.
mainLibrary :: HasPackageName a => a -> QualifiedComponentName
mainLibrary x = Library :/ (x & packageName & view isoViaShortText)

-- | The names of all external packages that the selected components depend upon
--
-- This includes both direct dependencies of the selected components, as well as
-- transitive dependencies via internal libraries.
--
-- Any selected components which do not exist will be ignored; no errors are thrown.
externalDependenciesOf
  :: Package
  -> [QualifiedComponentName]
  -- ^ Which components to select
  -> Set PackageName
externalDependenciesOf p searchRoot = runST do
  visited <- newSTRef mempty
  let alreadyVisited n = readSTRef visited <&> \v -> Set.member n v

  frontier <- newSTRef $ Set.fromList searchRoot
  let addToFrontier n = modifySTRef' frontier $ Set.insert n

  let
    consider qn = unless (alreadyVisited qn) $ addToFrontier qn
    considerLibrary name = consider (Library :/ name)

  result <- newSTRef mempty
  let addToResult n = modifySTRef' result $ Set.insert n

  whileJust (readSTRef frontier <&> Set.minView) \(x, frontier') -> do
    writeSTRef frontier frontier'
    modifySTRef' visited (Set.insert x)
    toListOf dependencyName (p, x) & traverse_ \case
      InternalDependencyName name -> considerLibrary name
      ExternalDependencyName name -> addToResult name

  readSTRef result
