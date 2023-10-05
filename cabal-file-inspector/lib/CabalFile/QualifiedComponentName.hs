module CabalFile.QualifiedComponentName where

import CabalFile.ComponentName
import CabalFile.ComponentType
import Essentials
import Optics

data QualifiedComponentName = (:/)
  { tag :: ComponentType
  , name :: ComponentName
  }
  deriving stock (Eq, Ord, Show)

instance ComponentTypeOptic A_Lens QualifiedComponentName where
  componentType = lens (.tag) \x y -> x {tag = y}
