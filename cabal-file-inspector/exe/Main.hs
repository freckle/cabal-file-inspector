module Main (main) where

import Control.Monad (join)
import Data.Function (flip)
import Essentials
import Exe.Command.Dependencies qualified as Dependencies
import Options.Applicative
import Prelude (IO)

main :: IO ()
main =
  join $
    execParser $
      flip info (progDesc "Cabal file inspector") $
        (<**> helper) $
          hsubparser $
            command "dependencies" $
              info Dependencies.command (progDesc "List the packages that a build depends on")
