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

import Gram.CST (Gram(..), AnnotatedPattern(..), PatternElement(..), Path(..), PathSegment(..), Node(..), Relationship(..), SubjectPattern(..), SubjectData(..), Identifier(..), Symbol(..))

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
registerElement (PEPath path) = registerPath path
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

registerPath :: Path -> State ValidationState ()
registerPath (Path start segments) = do
  registerNode start
  mapM_ registerSegment segments

registerSegment :: PathSegment -> State ValidationState ()
registerSegment (PathSegment rel nextNode) = do
  registerRelationship rel
  registerNode nextNode

registerNode :: Node -> State ValidationState ()
registerNode (Node (Just (SubjectData (Just ident) _ _))) = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just info | symStatus info == StatusDefined -> 
      -- Nodes in paths are usually references or first-time definitions.
      -- If it was explicitly defined as a pattern [a], using it as (a) is fine (reference).
      -- But if we try to define it again?
      -- Path notation treats nodes as references if they exist, or definitions if they don't.
      -- The only conflict is if we try to REDEFINE it with different properties, but we aren't checking property consistency yet.
      -- For US2, we mainly care about registering it if it's new.
      return () 
    _ -> do
      let info = SymbolInfo TypeNode StatusDefined Nothing
      put (Map.insert ident info syms, errs)
registerNode _ = return () -- Anonymous or reference-only nodes

registerRelationship :: Relationship -> State ValidationState ()
registerRelationship (Relationship _ (Just (SubjectData (Just ident) _ _))) = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just info | symStatus info == StatusDefined -> 
      -- Relationships in paths are definitions. 
      -- If (a)-[r]->(b) appears twice, is it a redefinition?
      -- If 'r' is identified, yes, it's a redefinition of 'r'.
      -- Unless it's the exact same relationship instance? 
      -- In Gram, (a)-[r]->(b) defines 'r'. Using 'r' again implies a new edge unless it's a reference [r].
      -- But [r] syntax isn't used inline in paths like that usually.
      -- Wait, `(a)-[r]->(b)` defines `r`. `(c)-[r]->(d)` would redefine `r` with new endpoints.
      -- So yes, duplicate definition.
      put (syms, DuplicateDefinition ident : errs)
    _ -> do
      let info = SymbolInfo TypeRelationship StatusDefined Nothing
      put (Map.insert ident info syms, errs)
registerRelationship _ = return ()

-- | Check references
checkReferences :: AnnotatedPattern -> State ValidationState ()
checkReferences (AnnotatedPattern _ elements) = 
  mapM_ checkElement elements

checkElement :: PatternElement -> State ValidationState ()
checkElement (PESubjectPattern sp) = checkSubjectPattern sp
checkElement (PEPath path) = checkPath path
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

checkPath :: Path -> State ValidationState ()
checkPath (Path start segments) = do
  checkNode start
  mapM_ checkSegment segments

checkSegment :: PathSegment -> State ValidationState ()
checkSegment (PathSegment rel nextNode) = do
  checkRelationship rel
  checkNode nextNode

checkNode :: Node -> State ValidationState ()
checkNode (Node (Just (SubjectData (Just ident) _ _))) = checkIdentifierRef ident
checkNode _ = return ()

checkRelationship :: Relationship -> State ValidationState ()
checkRelationship (Relationship _ (Just (SubjectData (Just ident) _ _))) = checkIdentifierRef ident
checkRelationship _ = return ()

checkIdentifierRef :: Identifier -> State ValidationState ()
checkIdentifierRef ident = do
  (syms, errs) <- get
  case Map.lookup ident syms of
    Just _ -> return () -- Defined or previously referenced
    Nothing -> put (syms, UndefinedReference ident : errs)
