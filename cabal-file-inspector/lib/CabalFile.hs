module CabalFile
  ( -- * Bytes → Cabal file
    parseCabalFile
  , pedanticCabalRead
  , CabalRead (..)
  , CabalReadFailure (..)
  , CabalReadIssue (..)
  , CabalReadIssues (..)
  , CabalReadException (..)
  , CabalFile

    -- * Cabal file → Package
  , applyCabalArguments
  , CabalArguments (..)
  , FlagName
  , Package

    -- * Package name
  , PackageName
  , HasPackageName (..)

    -- * Component identification
  , ComponentName
  , ComponentType (..)
  , QualifiedComponentName (..)
  , mainLibrary
  , componentsOfType
  , allComponents

    -- * Search for external dependencies
  , externalDependenciesOf

    -- * Text
  , toText
  , fromText
  , IsText
  )
where

import CabalFile.CabalFile
import CabalFile.ComponentName
import CabalFile.ComponentType
import CabalFile.FlagName
import CabalFile.Package
import CabalFile.PackageName
import CabalFile.QualifiedComponentName
import CabalFile.Text
