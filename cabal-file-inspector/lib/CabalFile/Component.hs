module CabalFile.Component where

import Cabal.Benchmark qualified
import Cabal.BuildInfo qualified as Cabal (BuildInfo)
import Cabal.Component qualified
import Cabal.Component qualified as Cabal (Component)
import Cabal.Executable qualified
import Cabal.ForeignLib qualified
import Cabal.Library qualified
import Cabal.LibraryName qualified
import Cabal.Optics qualified
import Cabal.TestSuite qualified
import CabalFile.CabalOptic
import CabalFile.ComponentType
import CabalFile.DependencyName
import CabalFile.PackageName
import CabalFile.QualifiedComponentName
import CabalFile.Text
import Data.Foldable (foldMap)
import Essentials
import Optics

data Component = Component {id :: QualifiedComponentName, buildInfo :: Cabal.BuildInfo}
  deriving stock (Eq, Show)

class ComponentOptic opticKind s | s -> opticKind where
  component :: Optic' opticKind NoIx s Component

componentBuildInfo :: Lens' Component Cabal.BuildInfo
componentBuildInfo = lens (.buildInfo) \x y -> x {buildInfo = y}

makeComponent :: PackageName -> Cabal.Component -> Component
makeComponent pkgName = \case
  Cabal.Component.CLib x ->
    Component
      { id =
          Library :/ case x.libName of
            Cabal.LibraryName.LMainLibName -> view isoViaShortText pkgName
            Cabal.LibraryName.LSubLibName y -> review cabal y
      , buildInfo = x.libBuildInfo
      }
  Cabal.Component.CFLib x ->
    Component
      { id = ForeignLibrary :/ review cabal x.foreignLibName
      , buildInfo = x.foreignLibBuildInfo
      }
  Cabal.Component.CExe x ->
    Component
      { id = Executable :/ review cabal x.exeName
      , buildInfo = x.buildInfo
      }
  Cabal.Component.CTest x ->
    Component
      { id = Test :/ review cabal x.testName
      , buildInfo = x.testBuildInfo
      }
  Cabal.Component.CBench x ->
    Component
      { id = Benchmark :/ review cabal x.benchmarkName
      , buildInfo = x.benchmarkBuildInfo
      }

instance DependencyNameOptic A_Fold (PackageNamingInfo, Component) where
  dependencyName = folding \(context, c) ->
    c
      & view (componentBuildInfo % Cabal.Optics.buildInfo_targetBuildDepends)
      & foldMap (\dependency -> toListOf dependencyName (context, dependency))
