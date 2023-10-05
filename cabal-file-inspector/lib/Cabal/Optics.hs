{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | Trivial optics for types in the Cabal library
module Cabal.Optics where

import Cabal.BuildInfo (BuildInfo)
import Cabal.BuildInfo qualified
import Cabal.Dependency (Dependency (Dependency))
import Cabal.Dependency qualified
import Cabal.Flag (FlagName)
import Cabal.Flag qualified
import Cabal.GenericPackageDescription (GenericPackageDescription)
import Cabal.GenericPackageDescription qualified
import Cabal.LibraryName (LibraryName)
import Cabal.NonEmptySet (NonEmptySet)
import Cabal.NonEmptySet qualified
import Cabal.PackageDescription (PackageDescription)
import Cabal.PackageDescription qualified
import Cabal.PackageId (PackageIdentifier)
import Cabal.PackageId qualified
import Cabal.PackageName (PackageName)
import Cabal.PackageName qualified
import Cabal.ShortText (ShortText)
import Cabal.ShortText qualified
import Cabal.UnqualComponentName (UnqualComponentName)
import Cabal.UnqualComponentName qualified
import Data.List.NonEmpty (NonEmpty)
import Essentials
import Optics
import Prelude (String)

packageDescription_name :: Lens' PackageDescription PackageName
packageDescription_name =
  packageDescription_packageIdentifier
    % packageIdentifier_name

genericPackageDescription_name :: Lens' GenericPackageDescription PackageName
genericPackageDescription_name =
  genericPackageDescription_packageDescription
    % packageDescription_name

genericPackageDescription_packageDescription
  :: Lens' GenericPackageDescription PackageDescription
genericPackageDescription_packageDescription =
  lens
    (.packageDescription)
    \x y -> x {Cabal.GenericPackageDescription.packageDescription = y}

packageDescription_packageIdentifier
  :: Lens' PackageDescription PackageIdentifier
packageDescription_packageIdentifier =
  lens
    (.package)
    \x y -> x {Cabal.PackageDescription.package = y}

packageIdentifier_name :: Lens' PackageIdentifier PackageName
packageIdentifier_name =
  lens
    (.pkgName)
    \x y -> x {Cabal.PackageId.pkgName = y}

componentName_shortText :: Iso' UnqualComponentName ShortText
componentName_shortText =
  iso
    Cabal.UnqualComponentName.unUnqualComponentNameST
    $ Cabal.UnqualComponentName.mkUnqualComponentName . Cabal.ShortText.fromShortText

componentName_string :: Iso' UnqualComponentName String
componentName_string =
  iso
    Cabal.UnqualComponentName.unUnqualComponentName
    Cabal.UnqualComponentName.mkUnqualComponentName

packageName_shortText :: Iso' PackageName ShortText
packageName_shortText =
  iso
    Cabal.PackageName.unPackageNameST
    Cabal.PackageName.mkPackageNameST

packageName_string :: Iso' PackageName String
packageName_string =
  iso
    Cabal.PackageName.unPackageName
    Cabal.PackageName.mkPackageName

buildInfo_targetBuildDepends :: Lens' BuildInfo [Dependency]
buildInfo_targetBuildDepends =
  lens
    (.targetBuildDepends)
    \x y -> x {Cabal.BuildInfo.targetBuildDepends = y}

dependency_packageName :: Lens' Dependency PackageName
dependency_packageName =
  lens
    Cabal.Dependency.depPkgName
    \(Dependency _ version libraries) packageName ->
      Dependency packageName version libraries

dependency_libraryNames :: Lens' Dependency (NonEmptySet LibraryName)
dependency_libraryNames =
  lens
    Cabal.Dependency.depLibraries
    \(Dependency packageName version _) libraries ->
      Dependency packageName version libraries

nonEmptySet_nonEmpty
  :: (Ord a, Ord b)
  => Iso (NonEmptySet a) (NonEmptySet b) (NonEmpty a) (NonEmpty b)
nonEmptySet_nonEmpty =
  iso
    Cabal.NonEmptySet.toNonEmpty
    Cabal.NonEmptySet.fromNonEmpty

flagName_string :: Iso' FlagName String
flagName_string = iso Cabal.Flag.unFlagName Cabal.Flag.mkFlagName
