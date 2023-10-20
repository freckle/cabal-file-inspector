module Exe.Compiler
  ( compilerReader
  )
where

import Cabal.Compiler
import Cabal.Version
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Essentials
import Exe.Text
import Options.Applicative
import Prelude (Int)

-- | Optparse reader for specifying which Haskell compiler we're using
--
-- This matters because a .cabal file can include conditionals based on information
-- about the compiler.
compilerReader :: ReadM CompilerId
compilerReader = maybeReader $ compilerFromText . T.pack

compilerFromText :: Text -> Maybe CompilerId
compilerFromText s =
  splitIn2 (== '-') s
    & maybe
      (Just $ CompilerId (compilerFlavorFromText s) nullVersion)
      \(flavorText, versionText) ->
        versionFromText versionText <&> CompilerId (compilerFlavorFromText flavorText)

compilerFlavorFromText :: Text -> CompilerFlavor
compilerFlavorFromText = T.unpack >>> classifyCompilerFlavor

versionFromText :: Text -> Maybe Version
versionFromText = T.split (== '.') >>> traverse versionPartFromText >>> fmap mkVersion

versionPartFromText :: Text -> Maybe Int
versionPartFromText = textReadOnly T.decimal
