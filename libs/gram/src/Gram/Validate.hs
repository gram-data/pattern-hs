{-# LANGUAGE OverloadedStrings #-}
module Gram.Validate 
  ( SymbolTable
  , SymbolInfo(..)
  , SymbolType(..)
  , DefinitionStatus(..)
  , PatternSignature(..)
  , ValidationEnv(..)
  , ValidationError(..)
  , validate
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Gram.CST (AnnotatedPattern, Identifier(..), Gram(..))

-- | The internal state used during validation.
type SymbolTable = Map Identifier SymbolInfo

data SymbolInfo = SymbolInfo
  { symType :: SymbolType      -- Node, Relationship, or Pattern
  , symStatus :: DefinitionStatus
  , symSignature :: Maybe PatternSignature -- For consistency checks
  } deriving (Show, Eq)

data SymbolType
  = TypeNode
  | TypeRelationship
  | TypePattern
  | TypeUnknown -- Inferred but not yet specific
  deriving (Show, Eq)

data DefinitionStatus
  = StatusDefined        -- Fully defined (e.g., [a])
  | StatusReferenced     -- Referenced but not yet defined
  | StatusImplicit       -- Implicitly defined (e.g., in a path)
  deriving (Show, Eq)

data PatternSignature = PatternSignature
  { sigLabels :: Set String
  , sigArity :: Int -- Number of elements
  } deriving (Show, Eq)

-- | The environment for validation.
data ValidationEnv = ValidationEnv
  { envCurrentPath :: [Identifier] -- For cycle detection
  } deriving (Show, Eq)

data ValidationError
  = DuplicateDefinition Identifier
  | UndefinedReference Identifier
  | SelfReference Identifier
  | InconsistentDefinition Identifier String
  | ImmutabilityViolation Identifier
  deriving (Show, Eq)

type ValidationResult = Either [ValidationError] ()

-- | Validate a parsed Gram AST.
validate :: Gram -> ValidationResult
validate _ = Right ()
