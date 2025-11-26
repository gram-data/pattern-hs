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

-- | Transform a CST Gram into a Core Pattern Subject
transformGram :: CST.Gram -> P.Pattern S.Subject
transformGram (CST.Gram record patterns) =
  case (record, patterns) of
    (Just props, []) -> 
      -- Only record
      P.Pattern (S.Subject (S.Symbol "") Set.empty props) []
    (Just props, pats) ->
      -- Record + Patterns: Record becomes root properties, patterns become elements
      P.Pattern (S.Subject (S.Symbol "") Set.empty props) (map transformPattern pats)
    (Nothing, [p]) ->
      -- Single pattern (common case)
      transformPattern p
    (Nothing, pats) ->
      -- Multiple patterns without root record: wrap in implicit root
      P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) (map transformPattern pats)

transformPattern :: CST.AnnotatedPattern -> P.Pattern S.Subject
transformPattern (CST.AnnotatedPattern _ elements) =
  case elements of
    [el] -> transformElement el
    (first:rest) -> 
      -- Multiple elements: First acts as container/root of the sequence (legacy behavior preserved)
      -- OR we should wrap them? The existing parser treated comma-separated lists by nesting:
      -- "a, b" -> Pattern a [Pattern b]
      -- Let's preserve this for now to pass tests.
      let root = transformElement first
          others = map transformElement rest
      in P.Pattern (P.value root) (P.elements root ++ others)
    [] -> P.Pattern (S.Subject (S.Symbol "") Set.empty Map.empty) []

transformElement :: CST.PatternElement -> P.Pattern S.Subject
transformElement (CST.PEPath path) = transformPath path
transformElement (CST.PESubjectPattern b) = transformSubjectPattern b
transformElement (CST.PEReference ident) =
  P.Pattern (S.Subject (transformIdentifier (Just ident)) Set.empty Map.empty) []

-- | Transform a path into a Pattern.
-- 
-- 1. Single Node: (a) -> Pattern a []
-- 2. Single Edge: (a)-[r]->(b) -> Pattern r [a, b]
-- 3. Walk: (a)-[r1]->(b)-[r2]->(c) -> Pattern walk [Pattern r1 [a, b], Pattern r2 [b, c]]
transformPath :: CST.Path -> P.Pattern S.Subject
transformPath (CST.Path startNode segments) =
  case segments of
    [] -> transformNode startNode
    [seg] -> 
      -- Single Edge case: Return the edge pattern directly
      -- (a)-[r]->(b) becomes [r | a, b]
      let left = transformNode startNode
          right = transformNode (CST.segmentNode seg)
          rel = transformRelationship (CST.segmentRel seg)
      in P.Pattern (P.value rel) [left, right]
    _ -> 
      -- Walk case (multiple segments): Return a Walk Pattern containing edges
      -- (a)-[r1]->(b)-[r2]->(c) becomes [walk | [r1 | a, b], [r2 | b, c]]
      let edges = constructWalkEdges startNode segments
          -- Use a specific label for Walk container to distinguish it
          walkSubject = S.Subject (S.Symbol "") (Set.singleton "Gram.Walk") Map.empty
      in P.Pattern walkSubject edges

-- | Construct a list of Edge Patterns from a start node and path segments.
constructWalkEdges :: CST.Node -> [CST.PathSegment] -> [P.Pattern S.Subject]
constructWalkEdges _ [] = []
constructWalkEdges leftNode (seg:rest) =
  let rightNode = CST.segmentNode seg
      leftP = transformNode leftNode
      rightP = transformNode rightNode
      relP = transformRelationship (CST.segmentRel seg)
      -- Create self-contained edge: [rel | left, right]
      edge = P.Pattern (P.value relP) [leftP, rightP]
  in edge : constructWalkEdges rightNode rest

transformNode :: CST.Node -> P.Pattern S.Subject
transformNode (CST.Node subjData) =
  let subj = maybe emptySubject transformSubjectData subjData
  in P.Pattern subj []

transformSubjectPattern :: CST.SubjectPattern -> P.Pattern S.Subject
transformSubjectPattern (CST.SubjectPattern subjData nested) =
  let subj = maybe emptySubject transformSubjectData subjData
      nestedPats = map transformElement nested
  in P.Pattern subj nestedPats

transformRelationship :: CST.Relationship -> P.Pattern S.Subject
transformRelationship (CST.Relationship _ _ subjData) =
  -- Arrow string is currently ignored in Pattern Subject (as per design)
  let subj = maybe emptySubject transformSubjectData subjData
  in P.Pattern subj []

transformSubjectData :: CST.SubjectData -> S.Subject
transformSubjectData (CST.SubjectData ident labels props) =
  S.Subject
    (transformIdentifier ident)
    labels
    props

transformIdentifier :: Maybe CST.Identifier -> S.Symbol
transformIdentifier Nothing = S.Symbol ""
transformIdentifier (Just (CST.IdentSymbol (CST.Symbol s))) = S.Symbol s
transformIdentifier (Just (CST.IdentString s)) = S.Symbol s
transformIdentifier (Just (CST.IdentInteger i)) = S.Symbol (show i)

emptySubject :: S.Subject
emptySubject = S.Subject (S.Symbol "") Set.empty Map.empty
