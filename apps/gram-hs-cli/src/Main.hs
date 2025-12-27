{-# LANGUAGE OverloadedStrings #-}
module Main where

import GramHs.CLI (runCLI)
import System.Exit (exitWith)

main :: IO ()
main = do
  exitCode <- runCLI
  exitWith exitCode

