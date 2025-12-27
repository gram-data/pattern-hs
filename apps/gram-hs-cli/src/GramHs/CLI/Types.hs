{-# LANGUAGE OverloadedStrings #-}
module GramHs.CLI.Types
  ( OutputFormat(..)
  ) where

data OutputFormat
  = FormatJSON
  | FormatGram
  | FormatDebug
  deriving (Eq, Show)

