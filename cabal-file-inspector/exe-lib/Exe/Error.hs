module Exe.Error where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Essentials
import System.Exit (exitFailure)
import System.IO (stderr)
import Prelude (IO)

-- | Error message destined for printing to the terminal
newtype Error = Error {text :: Text}

-- | Print error texts one-per-line on the standard error stream, then exit with a failure code
errorQuit :: NonEmpty Error -> IO a
errorQuit xs = traverse_ (Text.hPutStrLn stderr) ((.text) <$> xs) *> exitFailure
