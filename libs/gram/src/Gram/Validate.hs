{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.List (foldl')
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when)

import Gram.CST (Gram(..), AnnotatedPattern(..), PatternElement(..), SubjectPattern(..), SubjectData(..), Identifier(..), Symbol(..), Path(..))

-- | The internal state used during validation.
type SymbolTable = Map Identifier SymbolInfo

data SymbolInfo = SymbolInfo
  { symType :: SymbolType
  , symStatus :: DefinitionStatus
  , symSignature :: Maybe PatternSignature
  } deriving (Show, Eq)

data SymbolType
  = TypeNode
  | TypeRelationship
  | TypePattern
  | TypeUnknown
  deriving (Show, Eq)

data DefinitionStatus
  = StatusDefined
  | StatusReferenced
  | StatusImplicit
  deriving (Show, Eq)

data PatternSignature = PatternSignature
  { sigLabels :: Set String
  , sigArity :: Int
  } deriving (Show, Eq)

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

type ValidationState = (SymbolTable, [ValidationError])
type ValidateM a = State ValidationEnv a

-- | Initial state
emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty

emptyEnv :: ValidationEnv
emptyEnv = ValidationEnv []

-- | Validate a parsed Gram AST.
validate :: Gram -> Either [ValidationError] ()
validate (Gram _ patterns) = 
  let (_, errs) = execState (validatePatterns patterns) (emptySymbolTable, [])
  in if null errs then Right () else Left (reverse errs)

-- | Main validation loop state
-- State: (SymbolTable, [ValidationError])
validatePatterns :: [AnnotatedPattern] -> State ValidationState ()
validatePatterns pats = do
  -- Pass 1: Register all definitions
  mapM_ registerDefinition pats
  -- Pass 2: Check references
  mapM_ checkReferences pats

-- | Register definitions
registerDefinition :: AnnotatedPattern -> State ValidationState ()
registerDefinition (AnnotatedPattern _ elements) = 
  mapM_ registerElement elements

registerElement :: PatternElement -> State ValidationState ()
registerElement (PESubjectPattern sp) = registerSubjectPattern sp
registerElement (PEPath _) = return () -- Path defs handled in later pass (US2)
registerElement (PEReference _) = return () -- References don't define

registerSubjectPattern :: SubjectPattern -> State ValidationState ()
registerSubjectPattern (SubjectPattern maybeSubj elements) = do
  -- Register the subject itself if identified
  case maybeSubj of
    Just (SubjectData (Just ident) _ _) -> do
      (syms, errs) <- get
      case Map.lookup ident syms of
        Just info | symStatus info == StatusDefined -> 
          put (syms, DuplicateDefinition ident : errs)
        _ -> do
          let info = SymbolInfo TypePattern StatusDefined Nothing
          put (Map.insert ident info syms, errs)
    _ -> return ()
  
  -- Recurse into elements
  mapM_ registerElement elements

-- | Check references
checkReferences :: AnnotatedPattern -> State ValidationState ()
checkReferences (AnnotatedPattern _ elements) = 
  mapM_ checkElement elements

checkElement :: PatternElement -> State ValidationState ()
checkElement (PESubjectPattern sp) = checkSubjectPattern sp
checkElement (PEPath _) = return ()
checkElement (PEReference ident) = checkIdentifierRef ident

checkSubjectPattern :: SubjectPattern -> State ValidationState ()
checkSubjectPattern (SubjectPattern maybeSubj elements) = do
  -- Check for direct recursion if identified
  case maybeSubj of
    Just (SubjectData (Just ident) _ _) -> do
      -- Check elements for direct reference to ident
      let directRefs = [id | PEReference id <- elements]
      when (ident `elem` directRefs) $ do
        (syms, errs) <- get
        put (syms, SelfReference ident : errs)
    _ -> return ()

  mapM_ checkElement elements

checkIdentifierRef :: Identifier -> State ValidationState ()
checkIdentifierRef ident = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just _ -> return () -- Defined or previously referenced (implicit forward ref check is weak here, assuming definitions pass 1 caught all)
    Nothing -> put (syms, UndefinedReference ident : errs)

