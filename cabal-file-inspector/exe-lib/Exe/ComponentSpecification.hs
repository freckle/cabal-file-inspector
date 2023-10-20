module Exe.ComponentSpecification
  ( ComponentSetSpecification (..)
  , ComponentSpecification (..)
  , showComponent
  , componentsReader
  , resolveComponentSet
  )
where

import CabalFile
import Control.Monad.Validate
import Data.Bool (not)
import Data.Foldable (toList)
import Data.Function (flip)
import Data.List (elem, filter)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Essentials
import Exe.Error
import Exe.Text
import Map (Map)
import Map qualified
import Options.Applicative
import Set (Set)
import Set qualified
import Prelude (Bounded (..))

-- | An indication received on the command line of which component the user wants to consider
data ComponentSpecification
  = -- | Selects the "library" stanza, whose name is implicitly the name of the package
    MainLibrary
  | -- | Selects a component by type and name
    NamedComponent QualifiedComponentName
  deriving stock (Eq, Ord)

-- | Indication from the command line of the set of components the user wants to considder
--
-- If 'allComponents' is given, then 'components' only serves to indicate that the user
-- wishes to assert that the specified components exist.
data ComponentSetSpecification = ComponentSetSpecification
  { allComponents :: Bool
  -- ^ Consider every component in the package
  , components :: Set ComponentSpecification
  -- ^ A list of specific components to consider
  }

-- | Prints a component in the same format that 'componentsReader' reads
showComponent :: QualifiedComponentName -> Text
showComponent (t :/ n) = Text.concat [componentTypeName t, ":", toText n]

-- | Optparse reader in the form "(lib|flib|test|exe|bench):component-name"
componentsReader :: ReadM (Set QualifiedComponentName)
componentsReader = maybeReader $ componentSetFromText . Text.pack

componentSetFromText :: Text -> Maybe (Set QualifiedComponentName)
componentSetFromText =
  Text.split (flip (elem @[]) " ,")
    >>> filter (not . Text.null)
    >>> traverse componentFromText
    >>> fmap Set.fromList

componentFromText :: Text -> Maybe QualifiedComponentName
componentFromText a = do
  (t, n) <- splitIn2 (== ':') a
  componentTypeFromText t <&> (:/ fromText n)

componentTypeName :: ComponentType -> Text
componentTypeName = \case
  Library -> "lib"
  ForeignLibrary -> "flib"
  Test -> "test"
  Executable -> "exe"
  Benchmark -> "bench"

componentTypeMap :: Map Text ComponentType
componentTypeMap =
  Map.fromList $
    [minBound .. maxBound] <&> \t -> (componentTypeName t, t)

componentTypeFromText :: Text -> Maybe ComponentType
componentTypeFromText = flip Map.lookup componentTypeMap

-- | Given the user's abstract specification of which components they want, looks at the
--   package contents to come up with a concrete set of component names.
--
-- If any 'ComponentSpecification' refers to a component that does not exist in the package,
-- this action produces an error whose message indicates which components are missing.
resolveComponentSet
  :: MonadValidate (NonEmpty Error) m
  => ComponentSetSpecification
  -> Package
  -> m (Set QualifiedComponentName)
resolveComponentSet setSpec package =
  do
    let
      all = allComponents package

      wanted =
        setSpec.components & Set.map \case
          MainLibrary -> mainLibrary package
          NamedComponent x -> x

      missing = wanted Set.\\ all

    missing
      & Set.toList
      & nonEmpty
      & traverse_ (refute . pure . missingComponentsError)

    pure if setSpec.allComponents then all else wanted

missingComponentsError :: NonEmpty QualifiedComponentName -> Error
missingComponentsError missing =
  Error $
    Text.concat
      [ "Missing components: "
      , Text.intercalate ", " (showComponent <$> toList missing)
      ]
