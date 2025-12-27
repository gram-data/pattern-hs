{-# LANGUAGE OverloadedStrings #-}
module GramHs.CLI.Commands.Generate
  ( GenerateOptions(..)
  , generateOptions
  , runGenerate
  ) where

import Options.Applicative
import GramHs.CLI.Types (OutputFormat(..))
import qualified GramHs.CLI.Output as Output
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import System.Exit (ExitCode(..))
import System.Random (StdGen, mkStdGen, randomR)

data GeneratorType
  = GenPattern
  | GenGraph
  | GenSuite
  | GenProperty
  deriving (Show, Eq)

data GenerateOptions = GenerateOptions
  { generateType :: GeneratorType
  , generateCount :: Int
  , generateSeed :: Maybe Int
  , generateComplexity :: String
  , generateFormat :: OutputFormat
  } deriving (Show)

generateOptions :: Parser GenerateOptions
generateOptions = GenerateOptions
  <$> generatorTypeOption
  <*> countOption
  <*> seedOption
  <*> complexityOption
  <*> formatOption

generatorTypeOption :: Parser GeneratorType
generatorTypeOption = option (maybeReader parseGeneratorType)
  ( long "type"
  <> short 't'
  <> metavar "TYPE"
  <> value GenPattern
  <> help "Generator type: pattern, graph, suite, or property (default: pattern)"
  )

parseGeneratorType :: String -> Maybe GeneratorType
parseGeneratorType "pattern" = Just GenPattern
parseGeneratorType "graph" = Just GenGraph
parseGeneratorType "suite" = Just GenSuite
parseGeneratorType "property" = Just GenProperty
parseGeneratorType _ = Nothing

countOption :: Parser Int
countOption = option auto
  ( long "count"
  <> short 'c'
  <> metavar "N"
  <> value 1
  <> help "Number of items to generate (default: 1)"
  )

seedOption :: Parser (Maybe Int)
seedOption = optional $ option auto
  ( long "seed"
  <> short 's'
  <> metavar "SEED"
  <> help "Random seed for deterministic generation"
  )

complexityOption :: Parser String
complexityOption = strOption
  ( long "complexity"
  <> metavar "LEVEL"
  <> value "basic"
  <> help "Complexity level: minimal, basic, standard, complex, adversarial (default: basic)"
  )

formatOption :: Parser OutputFormat
formatOption = option (maybeReader parseFormatStr)
  ( long "format"
  <> short 'f'
  <> metavar "FORMAT"
  <> value FormatJSON
  <> help "Output format: json, gram, or debug (default: json)"
  )

parseFormatStr :: String -> Maybe OutputFormat
parseFormatStr "json" = Just FormatJSON
parseFormatStr "gram" = Just FormatGram
parseFormatStr "debug" = Just FormatDebug
parseFormatStr _ = Nothing

runGenerate :: GenerateOptions -> IO ExitCode
runGenerate opts = do
  let seed = maybe 42 id (generateSeed opts)
  let gen = mkStdGen seed
  
  case generateType opts of
    GenPattern -> do
      let patterns = take (generateCount opts) $ generatePatterns gen (generateComplexity opts)
      mapM_ (Output.formatOutput (generateFormat opts)) patterns
      return ExitSuccess
    _ -> do
      Output.formatError (generateFormat opts) "Generator type not yet implemented"
      return (ExitFailure 3)

generatePatterns :: StdGen -> String -> [Pattern.Pattern Subject.Subject]
generatePatterns gen complexity = 
  let (_, gen') = randomR (1 :: Int, 5 :: Int) gen
      subject = Subject.Subject (Subject.Symbol "gen") mempty mempty
  in Pattern.Pattern subject [] : generatePatterns gen' complexity

