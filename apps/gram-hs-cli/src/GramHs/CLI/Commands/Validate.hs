{-# LANGUAGE OverloadedStrings #-}
module GramHs.CLI.Commands.Validate
  ( ValidateOptions(..)
  , validateOptions
  , runValidate
  ) where

import Options.Applicative
import GramHs.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical)
import qualified GramHs.CLI.Output as Output
import System.Exit (ExitCode(..))
import System.Directory (doesDirectoryExist, listDirectory)

data ValidateOptions = ValidateOptions
  { validateTestSuite :: FilePath
  , validateRunner :: Maybe String
  , validateOutputOptions :: OutputOptions
  } deriving (Show)

validateOptions :: Parser ValidateOptions
validateOptions = ValidateOptions
  <$> strArgument (metavar "TEST-SUITE" <> help "Test suite directory or file")
  <*> optional (strOption (long "runner" <> metavar "COMMAND" <> help "External command to run tests against"))
  <*> outputOptionsParser

runValidate :: ValidateOptions -> IO ExitCode
runValidate opts = do
  let outputOpts = enforceDeterministicCanonical (validateOutputOptions opts)
  exists <- doesDirectoryExist (validateTestSuite opts)
  if exists
    then do
      files <- listDirectory (validateTestSuite opts)
      putStrLn $ "Found " ++ show (length files) ++ " test files"
      -- TODO: Implement test suite validation
      return ExitSuccess
    else do
      Output.formatError FormatJSON outputOpts ("Test suite not found: " ++ validateTestSuite opts)
      return (ExitFailure 2)

