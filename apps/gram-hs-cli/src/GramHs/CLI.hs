{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GramHs.CLI
  ( runCLI
  , CLICommand(..)
  ) where

import Options.Applicative
import qualified GramHs.CLI.Commands.Parse as Parse
import qualified GramHs.CLI.Commands.Match as Match
import qualified GramHs.CLI.Commands.Transform as Transform
import qualified GramHs.CLI.Commands.Generate as Generate
import qualified GramHs.CLI.Commands.Validate as Validate
import qualified GramHs.CLI.Commands.Convert as Convert
import System.Exit (ExitCode(..))

data CLICommand
  = Parse Parse.ParseOptions
  | Match Match.MatchOptions
  | Transform Transform.TransformOptions
  | Generate Generate.GenerateOptions
  | Validate Validate.ValidateOptions
  | Convert Convert.ConvertOptions
  deriving (Show)

runCLI :: IO ExitCode
runCLI = do
  cmd <- execParser cliInfo
  case cmd of
    Parse opts -> Parse.runParse opts
    Match opts -> Match.runMatch opts
    Transform opts -> Transform.runTransform opts
    Generate opts -> Generate.runGenerate opts
    Validate opts -> Validate.runValidate opts
    Convert opts -> Convert.runConvert opts

cliInfo :: ParserInfo CLICommand
cliInfo = info (helper <*> commandParser) fullDesc

commandParser :: Parser CLICommand
commandParser = hsubparser
  ( command "parse" (info (Parse <$> Parse.parseOptions) (progDesc "Parse gram notation and output canonical representation"))
  <> command "match" (info (Match <$> Match.matchOptions) (progDesc "Execute pattern matching and output bindings"))
  <> command "transform" (info (Transform <$> Transform.transformOptions) (progDesc "Apply pattern transformations"))
  <> command "generate" (info (Generate <$> Generate.generateOptions) (progDesc "Generate test data and patterns"))
  <> command "validate" (info (Validate <$> Validate.validateOptions) (progDesc "Run conformance test suites"))
  <> command "convert" (info (Convert <$> Convert.convertOptions) (progDesc "Convert between different representations"))
  )

