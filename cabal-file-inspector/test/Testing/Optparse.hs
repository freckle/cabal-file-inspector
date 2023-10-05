module Testing.Optparse where

import Essentials
import Options.Applicative
  ( Parser
  , ParserResult (CompletionInvoked, Failure, Success)
  , execParserPure
  , info
  , prefs
  )
import Test.HUnit (assertFailure)
import Prelude (IO, String, show)

parse :: Parser a -> [String] -> IO a
parse p args = case execParserPure (prefs mempty) (info p mempty) args of
  Success x -> pure x
  Failure x -> assertFailure (show x)
  CompletionInvoked x -> assertFailure (show x)
