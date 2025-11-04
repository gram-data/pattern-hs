-- | Main module for the Pattern library.
-- 
-- This module re-exports the core Pattern functionality for convenient use.
-- See individual modules for detailed documentation.
module Pattern
  ( module Pattern.Core
  , module Pattern.Views
  , module Pattern.Graph
  , module Pattern.Morphisms
  ) where

import Pattern.Core
import Pattern.Views
import Pattern.Graph
import Pattern.Morphisms

