{-# LANGUAGE OverloadedStrings #-}
module GramHs.CLI.Types
  ( OutputFormat(..)
  , OutputOptions(..)
  , defaultOutputOptions
  , enforceDeterministicCanonical
  , outputOptionsParser
  ) where

import Options.Applicative

-- | Output format options for CLI commands
data OutputFormat
  = FormatJSON
  | FormatGram
  | FormatDebug
  deriving (Eq, Show)

-- | Options for controlling output formatting behavior
--
-- These options enable deterministic, comparable output for equivalence checking
-- when porting to other languages.
--
-- @since 0.1.0
data OutputOptions = OutputOptions
  { valueOnly :: Bool      -- ^ Output only the result value without metadata wrapper
  , deterministic :: Bool -- ^ Use fixed values for metadata (timestamps, hashes)
  , canonical :: Bool     -- ^ Sort JSON keys alphabetically at all nesting levels
  } deriving (Eq, Show)

-- | Default output options (all flags disabled)
--
-- @since 0.1.0
defaultOutputOptions :: OutputOptions
defaultOutputOptions = OutputOptions
  { valueOnly = False
  , deterministic = False
  , canonical = False
  }

-- | Enforce that canonical is True when deterministic is True
--
-- This ensures that deterministic output always has sorted keys (FR-011).
--
-- @since 0.1.0
enforceDeterministicCanonical :: OutputOptions -> OutputOptions
enforceDeterministicCanonical opts
  | deterministic opts = opts { canonical = True }
  | otherwise = opts

-- | Parser for output formatting flags
--
-- Provides flags:
--   --value-only: Output only result value without metadata
--   --deterministic: Use fixed metadata values
--   --canonical: Sort JSON keys alphabetically
--
-- @since 0.1.0
outputOptionsParser :: Parser OutputOptions
outputOptionsParser = OutputOptions
  <$> switch
      ( long "value-only"
      <> help "Output only the result value without metadata wrapper"
      )
  <*> switch
      ( long "deterministic"
      <> help "Use fixed values for metadata (timestamps, hashes) for deterministic output"
      )
  <*> switch
      ( long "canonical"
      <> help "Sort JSON keys alphabetically at all nesting levels"
      )

