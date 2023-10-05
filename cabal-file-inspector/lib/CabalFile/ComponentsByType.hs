module CabalFile.ComponentsByType where

import Cabal.BuildInfo qualified as Cabal (BuildInfo)
import CabalFile.Component
import CabalFile.ComponentName
import CabalFile.ComponentType
import CabalFile.QualifiedComponentName
import Data.Foldable (foldMap, foldl')
import Data.Function (flip)
import Essentials
import Map (Map)
import Map qualified

newtype ComponentsByType = ComponentsByType
  { coerce :: Map ComponentType (Map ComponentName Cabal.BuildInfo)
  }

instance Semigroup ComponentsByType where
  x <> y = ComponentsByType $ Map.unionWith (<>) x.coerce y.coerce

instance Monoid ComponentsByType where
  mempty = ComponentsByType Map.empty

insert :: Component -> ComponentsByType -> ComponentsByType
insert c =
  (<>) $
    ComponentsByType $
      Map.singleton c.id.tag $
        Map.singleton c.id.name c.buildInfo

ofType :: ComponentType -> ComponentsByType -> [Component]
ofType t cbt =
  Map.lookup t cbt.coerce
    & maybe
      []
      ( Map.toList >>> fmap \(n, b) ->
          Component {id = t :/ n, buildInfo = b}
      )

toList :: ComponentsByType -> [Component]
toList =
  (.coerce)
    >>> Map.toList
    >>> foldMap \(t, m) ->
      Map.toList m <&> \(n, b) ->
        Component {id = t :/ n, buildInfo = b}

lookup :: QualifiedComponentName -> ComponentsByType -> Maybe Component
lookup qn =
  (.coerce)
    >>> Map.lookup qn.tag
    >>> (>>= Map.lookup qn.name)
    >>> (<&> \b -> Component {id = qn, buildInfo = b})

fromList :: [Component] -> ComponentsByType
fromList = foldl' (flip insert) mempty
