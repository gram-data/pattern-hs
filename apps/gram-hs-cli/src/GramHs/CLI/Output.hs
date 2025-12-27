{-# LANGUAGE OverloadedStrings #-}
module GramHs.CLI.Output
  ( formatOutput
  , formatError
  ) where

import GramHs.CLI.Types (OutputFormat(..))
import qualified GramHs.CLI.JSON as JSON
import qualified Gram.Serialize as Gram
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Data.Text.IO as TIO

formatOutput :: OutputFormat -> Pattern.Pattern Subject.Subject -> IO ()
formatOutput FormatJSON pat = TIO.putStrLn (JSON.patternToJSON pat)
formatOutput FormatGram pat = putStrLn (Gram.toGram pat)
formatOutput FormatDebug pat = print pat

formatError :: OutputFormat -> String -> IO ()
formatError FormatJSON err = TIO.putStrLn (JSON.errorToJSON err)
formatError FormatGram err = putStrLn ("Error: " ++ err)
formatError FormatDebug err = putStrLn ("Error: " ++ err)

