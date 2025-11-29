{-# LANGUAGE FlexibleContexts #-}
module Gram.Transform
  ( transformGram
  ) where

import qualified Gram.CST as CST
import qualified Pattern.Core as P
import qualified Subject.Core as S
import qualified Subject.Value as V
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, evalState, get, put)

type Transform = State Int

-- | Transform a CST Gram into a Core Pattern Subject
transformGram :: CST.Gram -> P.Pattern S.Subject
transformGram gram = evalState (transformGram' gram) 1

transformGram' :: CST.Gram -> Transform (P.Pattern S.Subject)
transformGram' (CST.Gram record patterns) =
  case (record, patterns) of
    (Just props, []) -> 
      return $ P.Pattern (S.Subject (S.Symbol "") Set.empty props) []
    (Just props, pats) -> do
      pats' <- mapM transformPattern pats
      return $ P.Pattern (S.Subject (S.Symbol "") Set.empty props) pats'
    (Nothing, [p]) ->
      transformPattern p
    (Nothing, pats) -> do
      pats' <- mapM transformPattern pats
      return $ P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) pats'

transformPattern :: CST.AnnotatedPattern -> Transform (P.Pattern S.Subject)
transformPattern (CST.AnnotatedPattern _ elements) =
  case elements of
    [el] -> transformElement el
    (first:rest) -> do
      root <- transformElement first
      others <- mapM transformElement rest
      return $ P.Pattern (P.value root) (P.elements root ++ others)
    [] -> return $ P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) []

transformElement :: CST.PatternElement -> Transform (P.Pattern S.Subject)
transformElement (CST.PEPath path) = transformPath path
transformElement (CST.PESubjectPattern b) = transformSubjectPattern b
transformElement (CST.PEReference ident) = do
  sym <- transformIdentifier (Just ident)
  return $ P.Pattern (S.Subject sym Set.empty Map.empty) []

-- | Transform a path into a Pattern.
-- 
-- 1. Single Node: (a) -> Pattern a []
-- 2. Single Edge: (a)-[r]->(b) -> Pattern r [a, b]
-- 3. Walk: (a)-[r1]->(b)-[r2]->(c) -> Pattern walk [Pattern r1 [a, b], Pattern r2 [b, c]]
transformPath :: CST.Path -> Transform (P.Pattern S.Subject)
transformPath (CST.Path startNode segments) =
  case segments of
    [] -> transformNode startNode
    [seg] -> do
      -- Single Edge case: Return the edge pattern directly
      -- (a)-[r]->(b) becomes [r | a, b]
      left <- transformNode startNode
      right <- transformNode (CST.segmentNode seg)
      rel <- transformRelationship (CST.segmentRel seg)
      return $ P.Pattern (P.value rel) [left, right]
    _ -> do
      -- Walk case (multiple segments): Return a Walk Pattern containing edges
      -- (a)-[r1]->(b)-[r2]->(c) becomes [walk | [r1 | a, b], [r2 | b, c]]
      edges <- constructWalkEdges startNode segments
      -- Use a specific label for Walk container to distinguish it
      let walkSubject = S.Subject (S.Symbol "") (Set.singleton "Gram.Walk") Map.empty
      return $ P.Pattern walkSubject edges

-- | Construct a list of Edge Patterns from a start node and path segments.
constructWalkEdges :: CST.Node -> [CST.PathSegment] -> Transform [P.Pattern S.Subject]
constructWalkEdges _ [] = return []
constructWalkEdges leftNode (seg:rest) = do
  let rightNode = CST.segmentNode seg
  leftP <- transformNode leftNode
  rightP <- transformNode rightNode
  relP <- transformRelationship (CST.segmentRel seg)
  -- Create self-contained edge: [rel | left, right]
  let edge = P.Pattern (P.value relP) [leftP, rightP]
  restEdges <- constructWalkEdges rightNode rest
  return (edge : restEdges)

transformNode :: CST.Node -> Transform (P.Pattern S.Subject)
transformNode (CST.Node subjData) = do
  subj <- maybe transformEmptySubject transformSubjectData subjData
  return $ P.Pattern subj []

transformSubjectPattern :: CST.SubjectPattern -> Transform (P.Pattern S.Subject)
transformSubjectPattern (CST.SubjectPattern subjData nested) = do
  subj <- maybe transformEmptySubject transformSubjectData subjData
  nestedPats <- mapM transformElement nested
  return $ P.Pattern subj nestedPats

transformRelationship :: CST.Relationship -> Transform (P.Pattern S.Subject)
transformRelationship (CST.Relationship _ subjData) = do
  -- Arrow string is currently ignored in Pattern Subject (as per design)
  subj <- maybe transformEmptySubject transformSubjectData subjData
  return $ P.Pattern subj []

transformSubjectData :: CST.SubjectData -> Transform S.Subject
transformSubjectData (CST.SubjectData ident labels props) = do
  sym <- transformIdentifier ident
  return $ S.Subject
    sym
    labels
    props

transformIdentifier :: Maybe CST.Identifier -> Transform S.Symbol
transformIdentifier Nothing = generateId
transformIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = return $ S.Symbol s
transformIdentifier (Just (CST.IdentString s)) = return $ S.Symbol s
transformIdentifier (Just (CST.IdentInteger i)) = return $ S.Symbol (show i)

transformEmptySubject :: Transform S.Subject
transformEmptySubject = do
  sym <- generateId
  return $ S.Subject sym Set.empty Map.empty

generateId :: Transform S.Symbol
generateId = do
  i <- get
  put (i + 1)
  return $ S.Symbol ("#" ++ show i)
