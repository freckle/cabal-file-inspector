module CabalFile.ComponentType where

import Essentials
import Optics

data ComponentType
  = -- | __@libary@__ in a .cabal file
    Library
  | -- | __@test-suite@__ in a .cabal file
    Test
  | -- | __@executable@__ in a .cabal file
    Executable
  | -- | __@benchmark@__ in a .cabal file
    Benchmark
  | -- | __@foreign-library@__ in a .cabal file
    --
    -- Like a library, except that the built code is intended for
    -- consumption by a non-Haskell client.
    ForeignLibrary
  deriving stock (Eq, Ord, Enum, Bounded, Show)

-- | A data type that has some relationship to component types
class ComponentTypeOptic opticKind s | s -> opticKind where
  componentType :: Optic' opticKind NoIx s ComponentType
