module CabalFile.CabalOptic where

import Optics

-- | A class for types that correspond to types in the Cabal library.
--
-- Typically such a type is a newtype and @k@ is 'An_Iso'.
class
  CabalOptic opticKind s t a b
    | s -> opticKind a
    , t -> opticKind b
    , s b -> t
    , t a -> s
  where
  cabal :: Optic opticKind NoIx s t a b
