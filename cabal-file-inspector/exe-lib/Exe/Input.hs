module Exe.Input where

import Data.ByteString (ByteString, hGetContents)
import Data.Text (Text)
import Data.Text qualified as T
import Essentials
import System.Directory.OsPath (doesFileExist)
import System.File.OsPath qualified as IO
import System.IO (stdin)
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import Prelude (IO)

-- | One file path, represented in two ways
data FilePath fp = FilePath
  { text :: Text
  -- ^ Representation of the file path as text, which is not technically always how paths are, but a close enough idealization
  , os :: fp
  -- ^ Representation of the file path as whatever the operating system actually uses for file paths
  }
  deriving stock (Functor)

-- | Basic ways that a program might read file input
--
-- The @fp@ parameter is the type of the operating system's file paths.
--
-- The @m@ parameter is the monadic context in which file actions take place.
data Input fp m = Input
  { textFilePath :: Text -> Maybe fp
  -- ^ Interpret text as an operating system file path (this is generally expected to succeed)
  , readStandardInput :: m ByteString
  -- ^ Read the entire contents of stdin as a strict byte string
  , readFile :: fp -> m ByteString
  -- ^ Read the entire contents of a file as a strict byte string (might throw)
  , doesFileExist :: fp -> m Bool
  -- ^ Ask whether a file exists (if False, 'readFile' will throw)
  }

-- | The normal type of 'Input' for real life
regularInput :: Input OsPath IO
regularInput =
  Input
    { textFilePath = T.unpack >>> OsPath.encodeUtf
    , readStandardInput = hGetContents stdin
    , readFile = IO.readFile'
    , doesFileExist = doesFileExist
    }
