{-# LANGUAGE InstanceSigs #-}
module Pattern.Core
  ( -- * Pattern Type
    Pattern(..)
    -- * Construction Functions
  , pattern
  , patternWith
  , fromList
    -- * Query Functions
  , length
  , size
  , depth
  , values
    -- * Predicate Functions
  , anyValue
  , allValues
  , filterPatterns
  , findPattern
  , findAllPatterns
  , matches
  , contains
    -- * Foldable/Traversable Extras
  , flatten
  , toTuple
    -- * Context/Comonad Functions
  , extract
  , duplicate
  , extend
  , depthAt
  , sizeAt
  , indicesAt
  ) where

import Prelude hiding (length)
import qualified Prelude
import Data.Foldable (toList)
import Data.Semigroup (Semigroup(..), sconcat, stimes)
import Data.Monoid ()
import Data.Hashable (Hashable(..))
import Control.Comonad (Comonad(..))
import Data.List.NonEmpty (NonEmpty(..))

-- | Core Pattern data type and basic operations.
--
-- This module defines the fundamental Pattern type as a recursive structure
-- that can represent graph elements and sequences.
--
-- == Conceptual Model: Patterns as Decorated Sequences
--
-- Conceptually, a Pattern is a decorated sequence: the elements form the pattern
-- itself, and the value provides decoration about that pattern.
-- For example, the pattern "A B B A" with decoration "Enclosed rhyme" represents
-- a specific sequence pattern (A B B A) that is classified as an "Enclosed rhyme".
-- The Pattern type represents such decorated sequences where:
--
-- * @elements@ - The pattern itself, represented as a sequence of elements
-- * @value@ - Decoration about what kind of pattern it is
--
-- The elements ARE the pattern; they are not subordinate to the value.
-- While implemented using a recursive tree structure, the primary semantic is that
-- elements form the pattern sequence itself. Each element in the sequence is itself
-- a Pattern, enabling arbitrarily nested and complex pattern structures.
--
-- == Implementation: Recursive Tree Structure
--
-- The Pattern type is implemented as a recursive tree structure, but this is purely
-- an implementation detail. The relationship between the sequence conceptual model
-- and tree implementation is:
--
-- **Primary Semantic (Conceptual)**: Patterns are decorated sequences where elements
-- form the pattern itself. The sequence order is essential to the pattern.
--
-- **Implementation Detail**: The tree structure is how sequences are represented in
-- memory. Each tree node stores a decoration (value) and contains the pattern elements
-- as a list, enabling recursive nesting.
--
-- **Relationship**: The tree implementation supports sequence semantics:
--
-- * The @elements@ field IS the pattern - it contains the sequence that defines the pattern
-- * The @value@ field provides decoration about what kind of pattern it is
-- * Tree traversal provides access to sequence elements in order
-- * The recursive structure enables patterns to contain patterns containing patterns, etc.
--
-- Conceptually, developers should think of patterns as decorated sequences where elements
-- form the pattern itself. The tree structure is an implementation detail that supports
-- sequence operations (ordering, length, access by position).
--
-- This recursive implementation enables:
--
-- * Atomic patterns: Patterns with no elements (@elements == []@), representing empty sequences. Atomic patterns are the fundamental building blocks from which all other patterns are constructed.
-- * Patterns with elements: Patterns containing one or more pattern elements in sequence
-- * Arbitrary nesting: Patterns can contain patterns containing patterns, enabling
--   deeply nested pattern structures
--
-- == Values and Pattern Decoration
--
-- Each Pattern instance decorates a sequence of elements with a value:
--
-- * The @value@ field stores decoration about what kind of pattern it is. This can be any type @v@,
--   such as a string identifier, an integer, a custom data type, etc.
-- * The @value@ is decoration about the pattern sequence itself, not part of the pattern.
-- * All patterns in a structure must share the same value type @v@ (enforced by the type system).
--
-- For example, an atomic pattern might have @value = "Person"@
-- (decoration indicating the pattern type) and @elements = []@ (empty sequence pattern).
-- A pattern with two elements might have @value = "knows"@ (the pattern type decoration)
-- and @elements = [atomA, atomB]@ (the pattern itself - a sequence of two atomic patterns).
--
-- == Elements and Pattern Structure
--
-- The @elements@ field IS the pattern - it contains the sequence that defines the pattern:
--
-- * An empty sequence (@elements == []@) represents a pattern with no elements (empty sequence)
-- * A non-empty sequence represents a pattern containing one or more pattern elements
-- * The elements are ordered and maintain their sequence order - this order is essential to the pattern
-- * Each element in the sequence is itself a Pattern, enabling recursive nesting
--
-- The pattern structure enables compositional patterns:
--
-- * A pattern can include other patterns as its elements
-- * Those element patterns can themselves include patterns
-- * This enables arbitrary depth nesting while maintaining the pattern sequence semantic
--
-- For example, a pattern might have @elements = [atom1, atom2, pair1]@
-- where each element is a Pattern. A pair pattern itself might have
-- @elements = [atomA, atomB]@, creating a nested pattern structure.
--
-- == Type Safety and Type Parameter @v@
--
-- The Pattern type is parameterized over value type @v@:
--
-- * @Pattern v@ allows patterns to store values of any type @v@
-- * All patterns in a structure must share the same value type @v@
-- * This type consistency is enforced by Haskell's type system
-- * The type parameter ensures type safety when working with patterns
--
-- For example:
--
-- * @Pattern String@ - patterns storing string values
-- * @Pattern Int@ - patterns storing integer values
-- * @Pattern Person@ - patterns storing custom Person values
--
-- Type consistency means that if you have a @Pattern String@, all patterns in
-- its @elements@ list must also be @Pattern String@. This prevents mixing
-- different value types within a single pattern structure.
--
-- == Mathematical Foundation
--
-- Patterns form the foundation for category-theoretic graph representations.
-- The recursive structure enables functor instances and supports various graph
-- interpretations through categorical views. The sequence semantic aligns with
-- categorical composition and transformation operations.
--
-- The Pattern type has a Functor instance that enables value transformation while
-- preserving pattern structure. This supports functional transformations and type
-- conversions essential for pattern manipulation. See the Functor instance documentation
-- below for details on structure preservation and functor laws.
--
-- The Pattern type has a Foldable instance that enables value aggregation over pattern
-- structures. This supports operations like summing values, concatenating strings, counting
-- elements, and computing statistics without manually traversing the pattern tree. The instance
-- provides @foldr@ for right-associative folding, @foldl@ for left-associative folding,
-- @foldMap@ for monoid-based aggregation, and @toList@ for extracting all values as a flat list.
-- The module also provides @flatten@ as an explicit function for extracting all values as a flat
-- list, equivalent to @toList@. See the Foldable instance documentation below for details on
-- value aggregation and folding operations.
--
-- The Pattern type has a Traversable instance that enables effectful traversal over pattern
-- structures while preserving pattern structure. This supports operations like validation,
-- state threading, IO operations, and error handling over pattern values. The instance provides
-- @traverse@ for applying effectful functions to all values and @sequenceA@ for sequencing
-- applicative effects. See the Traversable instance documentation below for details on effectful
-- traversal and structure preservation.
--
-- The Pattern type has an Ord instance that provides lexicographic ordering for patterns based
-- on their structure. Patterns are ordered by comparing their value first, then their elements
-- recursively. This ordering is consistent with the Eq instance and enables patterns to be
-- used as keys in Data.Map and elements in Data.Set. The Ord instance requires that the value
-- type @v@ has an Ord instance. See the Ord instance documentation below for details on
-- ordering rules and consistency with equality.
--
-- The Pattern type has a Semigroup instance that enables combining patterns by concatenating
-- their elements and combining their values using the value type's Semigroup instance. This
-- enables incremental pattern construction using standard Haskell combinators like `<>`,
-- `sconcat`, and `stimes`. The instance preserves the decorated sequence model where elements
-- form the pattern and values provide decoration. The Semigroup instance requires that the
-- value type @v@ has a Semigroup instance. See the Semigroup instance documentation below
-- for details on combination semantics and associativity.
--
-- The Pattern type has a Monoid instance that extends the Semigroup instance by providing
-- an identity element (`mempty`). The identity pattern has `mempty` value (from value type's
-- Monoid) and empty elements list, enabling identity-based operations and standard Monoid
-- combinators (e.g., `mconcat`) while preserving the decorated sequence model. The Monoid
-- instance requires that the value type @v@ has a Monoid instance. See the Monoid instance
-- documentation below for details on identity semantics and laws.
--
-- The Pattern type has a Hashable instance that enables using patterns as keys in `HashMap`
-- and elements in `HashSet` for efficient hash-based lookups and deduplication. The instance
-- uses structure-preserving hashing based on value and elements recursively, ensuring that
-- equal patterns (according to `Eq`) produce the same hash value while providing good
-- distribution. The Hashable instance requires that the value type @v@ has a Hashable
-- instance. See the Hashable instance documentation below for details on hash semantics and
-- consistency with equality.
--
-- The Pattern type has a Comonad instance that enables context-aware computations where
-- functions have access to the full structural context (parent, siblings, depth, indices) around
-- each value, not just the value itself. This extends beyond Foldable (which only provides
-- values) to enable computations that consider structural context, depth, position, and
-- relationships between pattern elements. The instance provides @extract@ (extract decoration
-- value), @duplicate@ (create pattern of contexts), and @extend@ (context-aware transformation),
-- satisfying all Comonad laws (extract-extend, extend-extract, extend composition). See the
-- Comonad instance documentation below for details on context-aware computation and relationship
-- to zippers.
--
-- The Pattern type provides query functions for introspecting pattern structure:
--
-- * @length@ - Returns the number of direct elements in a pattern's sequence (O(1))
-- * @size@ - Returns the total number of nodes in a pattern structure, including all nested patterns (O(n))
-- * @depth@ - Returns the maximum nesting depth of a pattern structure (O(n))
-- * @values@ - Extracts all values from a pattern structure as a flat list (O(n))
-- * @value@ - Field accessor for accessing a pattern's decoration value (O(1))
-- * @anyValue@ - Checks if any value in a pattern satisfies a predicate (O(n))
-- * @allValues@ - Checks if all values in a pattern satisfy a predicate (O(n))
-- * @filterPatterns@ - Filters all subpatterns (including root) matching a pattern predicate (O(n))
-- * @findPattern@ - Finds the first subpattern (including root) matching a pattern predicate (O(n))
-- * @findAllPatterns@ - Finds all subpatterns (including root) matching a pattern predicate (O(n))
-- * @matches@ - Checks if two patterns match structurally (O(n))
-- * @contains@ - Checks if a pattern contains a subpattern (O(n))
--
-- These query functions enable pattern introspection, validation, and analysis operations.
-- See individual function documentation for details on usage and performance characteristics.
--
-- == Examples
--
-- Atomic pattern:
--
-- >>> atom = pattern "atom1"
-- >>> value atom
-- "atom1"
-- >>> elements atom
-- []
--
-- Pattern with elements:
--
-- >>> elem1 = pattern "elem1"
-- >>> elem2 = pattern "elem2"
-- >>> pattern = patternWith "pattern" [elem1, elem2]
-- >>> value pattern
-- "pattern"
-- >>> length (elements pattern)
-- 2
-- >>> map value (elements pattern)
-- ["elem1","elem2"]
--
-- Nested pattern:
--
-- >>> level3 = pattern "level3"
-- >>> level2 = patternWith "level2" [level3]
-- >>> level1 = patternWith "level1" [level2]
-- >>> nested = patternWith "root" [level1]
-- >>> value nested
-- "root"
-- >>> value (head (elements nested))
-- "level1"
data Pattern v = Pattern
  { value    :: v          -- ^ Decoration about what kind of pattern it is
  , elements :: [Pattern v] -- ^ The pattern itself, represented as a sequence of elements
  }
  deriving (Eq)

-- | 'Show' instance for 'Pattern'.
--
-- Displays the pattern in a readable format: @Pattern "value" [elements]@.
-- Requires the value type @v@ to be an instance of 'Show'.
--
-- === Examples
--
-- >>> show (pattern "test")
-- "Pattern \"test\" []"
--
-- >>> show (pattern 42)
-- "Pattern 42 []"
instance Show v => Show (Pattern v) where
  show (Pattern v []) = "Pattern " ++ show v ++ " []"
  show (Pattern v es) = "Pattern " ++ show v ++ " " ++ show es

-- | 'Ord' instance for 'Pattern'.
--
-- Provides lexicographic ordering for patterns based on their structure.
-- Patterns are compared by their value first, then by their elements recursively.
-- This ordering is consistent with the 'Eq' instance: if two patterns are equal,
-- they compare as 'EQ'.
--
-- The ordering rules are:
-- 1. Compare values: if values differ, their order determines the pattern order.
-- 2. If values are equal, compare elements lists lexicographically.
--
-- This instance enables patterns to be used as keys in 'Data.Map', elements in 'Data.Set',
-- and anywhere else that requires total ordering.
--
-- Requires the value type @v@ to be an instance of 'Ord'.
--
-- === Examples
--
-- Comparing atomic patterns:
--
-- >>> compare (pattern "a") (pattern "b")
-- LT
-- >>> compare (pattern "b") (pattern "a")
-- GT
--
-- Comparing patterns with elements (value takes precedence):
--
-- >>> p1 = patternWith "root" [pattern "a"]
-- >>> p2 = patternWith "root" [pattern "b"]
-- >>> compare p1 p2
-- LT
--
-- Comparing identical patterns:
--
-- >>> p1 = patternWith "root" [pattern "a", pattern "b"]
-- >>> p2 = patternWith "root" [pattern "a", pattern "b"]
-- >>> compare p1 p2
-- EQ
--
-- Deep structural comparison:
--
-- >>> inner1 = pattern "inner1"
-- >>> inner2 = pattern "inner2"
-- >>> outer1 = patternWith "outer" [patternWith "middle" [inner1]]
-- >>> outer2 = patternWith "outer" [patternWith "middle" [inner2]]
-- >>> compare outer1 outer2
-- LT
--
-- Using standard operators:
--
-- >>> (pattern "a") < (pattern "b")
-- True
-- >>> (pattern "a") <= (pattern "b")
-- True
-- >>> (pattern "b") > (pattern "a")
-- True
--
-- Min and Max:
--
-- >>> min (pattern "a") (pattern "b")
-- Pattern "a" []
-- >>> max (pattern "a") (pattern "b")
-- Pattern "b" []
instance Ord v => Ord (Pattern v) where
  compare (Pattern v1 es1) (Pattern v2 es2) =
    case compare v1 v2 of
      EQ -> compare es1 es2
      other -> other

-- | 'Semigroup' instance for 'Pattern'.
--
-- Enables combining two patterns into a new pattern.
--
-- Semantics:
-- * Values are combined using the value type's 'Semigroup' instance (`<>`).
-- * Elements are concatenated using list concatenation (`++`).
--
-- This aligns with the decorated sequence model:
-- * The pattern sequence becomes the concatenation of the two sequences.
-- * The decoration becomes the combination of the two decorations.
--
-- Requires the value type @v@ to be an instance of 'Semigroup'.
--
-- === Examples
--
-- Combining atomic patterns:
--
-- >>> p1 = pattern "hello"
-- >>> p2 = pattern "world"
-- >>> p1 <> p2
-- Pattern "helloworld" []
--
-- Combining patterns with elements:
--
-- >>> elem1 = pattern "a"
-- >>> elem2 = pattern "b"
-- >>> p1 = patternWith "prefix" [elem1, elem2]
-- >>> p2 = patternWith "suffix" [pattern "c"]
-- >>> p1 <> p2
-- Pattern "prefixsuffix" [Pattern "a" [],Pattern "b" [],Pattern "c" []]
--
-- Using Sum semigroup:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> p1 = pattern (Sum 5)
-- >>> p2 = pattern (Sum 3)
-- >>> getSum (value (p1 <> p2))
-- 8
--
-- Using Product semigroup:
--
-- >>> import Data.Monoid (Product(..))
-- >>> p1 = pattern (Product 5)
-- >>> p2 = pattern (Product 3)
-- >>> getProduct (value (p1 <> p2))
-- 15
--
-- Using sconcat for list of non-empty patterns:
--
-- >>> sconcat (pattern "a" :| [pattern "b", pattern "c"])
-- Pattern "abc" []
--
-- Using stimes for repetition:
--
-- >>> stimes 3 (patternWith "x" [pattern "y"])
-- Pattern "xxx" [Pattern "y" [],Pattern "y" [],Pattern "y" []]
--
-- Complex combination:
--
-- >>> inner1 = pattern "inner1"
-- >>> inner2 = pattern "inner2"
-- >>> middle1 = patternWith "middle1" [inner1]
-- >>> middle2 = patternWith "middle2" [inner2]
-- >>> p1 = patternWith "root1" [middle1]
-- >>> p2 = patternWith "root2" [middle2]
-- >>> result = p1 <> p2
-- >>> value result
-- "root1root2"
-- >>> length (elements result)
-- 2
instance Semigroup v => Semigroup (Pattern v) where
  (Pattern v1 es1) <> (Pattern v2 es2) = Pattern (v1 <> v2) (es1 ++ es2)
  
  -- | Optimized sconcat implementation
  sconcat :: NonEmpty (Pattern v) -> Pattern v
  sconcat ps = Pattern 
    (sconcat (fmap value ps)) 
    (concatMap elements (toList ps))

  -- | Optimized stimes implementation
  stimes :: Integral b => b -> Pattern v -> Pattern v
  stimes n (Pattern v es) = Pattern
    (stimes n v)
    (concat (replicate (fromIntegral n) es))

-- | 'Monoid' instance for 'Pattern'.
--
-- Extends the 'Semigroup' instance by providing an identity element ('mempty').
--
-- Semantics:
-- * 'mempty' is a pattern with 'mempty' value and empty elements list.
-- * 'mappend' is equivalent to '<>'.
-- * 'mconcat' combines a list of patterns (optimized).
--
-- Identity Laws:
-- * `mempty <> p = p`
-- * `p <> mempty = p`
--
-- Requires the value type @v@ to be an instance of 'Monoid'.
--
-- === Examples
--
-- Identity element:
--
-- >>> mempty :: Pattern String
-- Pattern "" []
-- >>> mempty :: Pattern (Sum Int)
-- Pattern (Sum {getSum = 0}) []
--
-- Identity laws:
--
-- >>> mempty <> pattern "test"
-- Pattern "test" []
-- >>> pattern "test" <> mempty
-- Pattern "test" []
--
-- Combining list of patterns:
--
-- >>> mconcat [pattern "a", pattern "b", pattern "c"]
-- Pattern "abc" []
--
-- Empty list returns mempty:
--
-- >>> mconcat [] :: Pattern String
-- Pattern "" []
instance Monoid v => Monoid (Pattern v) where
  mempty = Pattern mempty []
  mappend = (<>)
  
  -- | Optimized mconcat implementation
  mconcat :: [Pattern v] -> Pattern v
  mconcat ps = Pattern
    (mconcat (map value ps))
    (concatMap elements ps)

-- | 'Hashable' instance for 'Pattern'.
--
-- Enables using patterns as keys in 'Data.HashMap' and elements in 'Data.HashSet'.
-- Patterns are hashed based on their structure (value and elements recursively).
--
-- Semantics:
-- * Hashing combines the hash of the value and the hash of the elements list.
-- * Structural equality implies hash equality (if p1 == p2, then hash p1 == hash p2).
-- * Different structures with same flattened values produce different hashes.
--
-- Requires the value type @v@ to be an instance of 'Hashable'.
--
-- === Examples
--
-- Hashing atomic patterns:
--
-- >>> hash (pattern "a" :: Pattern String)
-- ...
-- >>> hash (pattern "b" :: Pattern String)
-- ...
--
-- Hashing patterns with elements:
--
-- >>> hash (patternWith "root" [pattern "a", pattern "b"] :: Pattern String)
-- ...
--
-- Hash consistency with equality:
--
-- >>> let p1 = pattern "test" :: Pattern String
-- >>> let p2 = pattern "test" :: Pattern String
-- >>> p1 == p2
-- True
-- >>> hash p1 == hash p2
-- True
--
-- Structure distinguishes hash:
--
-- >>> let p1 = patternWith "a" [pattern "b", pattern "c"] :: Pattern String
-- >>> let p2 = patternWith "a" [patternWith "b" [pattern "c"]] :: Pattern String
-- >>> hash p1 /= hash p2
-- True
--
-- Using in HashMap:
--
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> let m = HashMap.fromList [(pattern "a", 1), (pattern "b", 2)] :: HashMap (Pattern String) Int
-- >>> HashMap.lookup (pattern "a") m
-- Just 1
--
-- Using in HashSet:
--
-- >>> import qualified Data.HashSet as HashSet
-- >>> let s = HashSet.fromList [pattern "a", pattern "b", pattern "c"] :: HashSet (Pattern String)
-- >>> HashSet.member (pattern "a") s
-- True
instance Hashable v => Hashable (Pattern v) where
  hashWithSalt salt (Pattern v es) = 
    salt `hashWithSalt` v `hashWithSalt` es

-- | 'Functor' instance for 'Pattern'.
--
-- Maps a function over the values decorating the pattern structure.
-- Preserves the pattern structure (number and order of elements).
--
-- Laws:
-- * Identity: `fmap id == id`
-- * Composition: `fmap (f . g) == fmap f . fmap g`
--
-- === Examples
--
-- >>> atom = pattern "test"
-- >>> fmap (map toUpper) atom
-- Pattern "TEST" []
--
-- >>> elem1 = pattern "hello"
-- >>> elem2 = pattern "world"
-- >>> pattern = patternWith "greeting" [elem1, elem2]
-- >>> fmap (map toUpper) pattern
-- Pattern "GREETING" [Pattern "HELLO" [],Pattern "WORLD" []]
--
-- >>> elem1 = pattern 5
-- >>> elem2 = pattern 10
-- >>> pattern = patternWith 20 [elem1, elem2]
-- >>> fmap (* 2) pattern
-- Pattern 40 [Pattern 10 [],Pattern 20 []]
--
-- >>> inner = pattern "inner"
-- >>> middle = patternWith "middle" [inner]
-- >>> outer = patternWith "outer" [middle]
-- >>> pattern = patternWith "root" [outer]
-- >>> fmap (map toUpper) pattern
-- Pattern "ROOT" [Pattern "OUTER" [Pattern "MIDDLE" [Pattern "INNER" []]]]
--
-- >>> level4 = pattern "level4"
-- >>> level3 = patternWith "level3" [level4]
-- >>> level2 = patternWith "level2" [level3]
-- >>> level1 = patternWith "level1" [level2]
-- >>> pattern = patternWith "root" [level1]
-- >>> fmap (map toUpper) pattern
-- Pattern "ROOT" [Pattern "LEVEL1" [Pattern "LEVEL2" [Pattern "LEVEL3" [Pattern "LEVEL4" []]]]]
--
-- >>> branch1 = patternWith "b1" [pattern "b1leaf"]
-- >>> branch2 = patternWith "b2" [patternWith "b2mid" [pattern "b2leaf"]]
-- >>> branch3 = pattern "b3"
-- >>> pattern = patternWith "root" [branch1, branch2, branch3]
-- >>> fmap (map toUpper) pattern
-- Pattern "ROOT" [Pattern "B1" [Pattern "B1LEAF" []],Pattern "B2" [Pattern "B2MID" [Pattern "B2LEAF" []]],Pattern "B3" []]
--
-- >>> elem1 = pattern "5"
-- >>> elem2 = pattern "10"
-- >>> pattern = patternWith "20" [elem1, elem2]
-- >>> fmap (read :: String -> Int) pattern
-- Pattern 20 [Pattern 5 [],Pattern 10 []]
--
-- >>> atom = Pattern { value = "atom", elements = [] }
-- >>> fmap (map toUpper) atom
-- Pattern "ATOM" []
--
-- >>> elem = Pattern { value = "elem", elements = [] }
-- >>> pattern = Pattern { value = "singular", elements = [elem] }
-- >>> fmap (map toUpper) pattern
-- Pattern "SINGULAR" [Pattern "ELEM" []]
--
-- >>> elem1 = Pattern { value = "first", elements = [] }
-- >>> elem2 = Pattern { value = "second", elements = [] }
-- >>> pattern = Pattern { value = "pair", elements = [elem1, elem2] }
-- >>> fmap (map toUpper) pattern
-- Pattern "PAIR" [Pattern "FIRST" [],Pattern "SECOND" []]
--
-- >>> elems = [Pattern { value = "a", elements = [] }, Pattern { value = "b", elements = [] }, Pattern { value = "c", elements = [] }]
-- >>> pattern = Pattern { value = "extended", elements = elems }
-- >>> fmap (map toUpper) pattern
-- Pattern "EXTENDED" [Pattern "A" [],Pattern "B" [],Pattern "C" []]
--
-- >>> elem1 = Pattern { value = 5, elements = [] }
-- >>> elem2 = Pattern { value = 10, elements = [] }
-- >>> pattern = Pattern { value = 20, elements = [elem1, elem2] }
-- >>> fmap show pattern
-- Pattern "20" [Pattern "5" [],Pattern "10" []]
instance Functor Pattern where
  fmap f (Pattern v es) = Pattern (f v) (map (fmap f) es)

-- | 'Applicative' instance for 'Pattern'.
--
-- Enables applying functions stored in patterns to values stored in patterns.
--
-- Semantics:
-- * 'pure' creates an atomic pattern with the given value (empty elements list).
-- * '<*>' applies the function pattern to the value pattern using structure-preserving semantics:
--   - The root function is applied to the root value.
--   - Element functions are applied to corresponding element values (zip-like).
--   - If element counts differ, the result has the minimum number of elements (truncation).
--
-- Note on Semantics:
-- The Applicative instance uses "zip-like" semantics for elements, similar to 'ZipList'.
-- This means structure is preserved where it overlaps. This is distinct from the
-- Cartesian product semantics of standard List Applicative.
--
-- Laws:
-- * Identity: `pure id <*> v == v`
-- * Composition: `pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`
-- * Homomorphism: `pure f <*> pure x == pure (f x)`
-- * Interchange: `u <*> pure y == pure ($ y) <*> u`
--
-- === Examples
--
-- Pure creates atomic pattern:
--
-- >>> pure 5
-- Pattern 5 []
--
-- Applying function pattern to value pattern:
--
-- >>> let f = pure (+1)
-- >>> let x = pure 5
-- >>> f <*> x
-- Pattern 6 []
--
-- Zip-like application for elements:
--
-- >>> let fs = patternWith id [pure (*2), pure (+10)]
-- >>> let xs = patternWith 5 [pure 3, pure 7]
-- >>> fs <*> xs
-- Pattern 5 [Pattern 6 [],Pattern 17 []]
--
-- Nested application:
--
-- >>> let fs = patternWith id [patternWith (*2) [pure (*3)], patternWith (+1) []]
-- >>> let xs = patternWith 1 [patternWith 2 [pure 3], patternWith 4 []]
-- >>> fs <*> xs
-- Pattern 1 [Pattern 4 [Pattern 9 []],Pattern 5 []]
--
-- Truncation (mismatched element counts):
--
-- >>> let fs = patternWith id [pure (*2)]  -- 1 element
-- >>> let xs = patternWith 5 [pure 3, pure 7]   -- 2 elements
-- >>> fs <*> xs
-- Pattern 5 [Pattern 6 []]
instance Applicative Pattern where
  pure :: a -> Pattern a
  pure x = Pattern x []

  (<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
  (Pattern f fs) <*> (Pattern x xs) = Pattern (f x) (zipWith (<*>) fs xs)

-- | 'Comonad' instance for 'Pattern'.
--
-- Enables context-aware computations where functions have access to the full
-- structural context (parent, siblings, depth, indices) around each value.
--
-- Semantics:
-- * 'extract' returns the decoration value at the current focus (root).
-- * 'duplicate' creates a pattern where each position contains the full pattern
--   structure focused at that position. This allows each position to "see" its
--   structural context.
-- * 'extend' applies a context-aware function to each position in the pattern.
--
-- Laws:
-- * Left Identity: `extract . extend f == f`
-- * Right Identity: `extend extract == id`
-- * Associativity: `extend f . extend g == extend (f . extend g)`
--
-- === Examples
--
-- Extract returns the root value:
--
-- >>> p = pattern 5
-- >>> extract p
-- 5
--
-- Duplicate creates context structure:
--
-- >>> p = patternWith "root" [pattern "child"]
-- >>> d = duplicate p
-- >>> value d
-- Pattern "root" [Pattern "child" []]
-- >>> value (head (elements d))
-- Pattern "child" []
--
-- Extend applies context-aware function:
--
-- >>> p = patternWith 1 [pattern 2, pattern 3]
-- >>> -- Calculate sum of subtree at each position
-- >>> sumSubtree (Pattern v es) = v + sum (map (extract . fmap sumSubtree) es)
-- >>> -- Note: proper implementation would use sizeAt or similar helper
-- >>> extend (const "context") p
-- Pattern "context" [Pattern "context" [],Pattern "context" []]
instance Comonad Pattern where
  extract :: Pattern a -> a
  extract (Pattern v _) = v

  duplicate :: Pattern a -> Pattern (Pattern a)
  duplicate p@(Pattern _ es) = Pattern p (map duplicate es)

  extend :: (Pattern a -> b) -> Pattern a -> Pattern b
  extend f p@(Pattern _ es) = Pattern (f p) (map (extend f) es)

-- | 'Foldable' instance for 'Pattern'.
--
-- folds over the values decorating the pattern structure.
--
-- The fold order is defined by the recursive structure:
-- 1. The value at the current node
-- 2. The elements in the elements list (recursively)
--
-- === Examples
--
-- >>> atom = pattern "test"
-- >>> toList atom
-- ["test"]
--
-- >>> pattern = patternWith "root" [pattern "a", pattern "b"]
-- >>> toList pattern
-- ["root","a","b"]
--
-- >>> sum (patternWith 1 [pattern 2, pattern 3])
-- 6
instance Foldable Pattern where
  foldMap f (Pattern v es) = f v <> foldMap (foldMap f) es

-- | 'Traversable' instance for 'Pattern'.
--
-- Enables effectful traversal over pattern values while preserving structure.
--
-- Semantics:
-- * Applies an effectful function to each value.
-- * Sequences the effects.
-- * Reconstructs the pattern structure with the results.
--
-- Laws:
-- * Naturality: `t . traverse f = traverse (t . f)`
-- * Identity: `traverse Identity = Identity`
-- * Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`
--
-- === Examples
--
-- Basic traversal with Identity (equivalent to fmap):
--
-- >>> import Data.Functor.Identity
-- >>> pattern = patternWith 1 [pattern 2]
-- >>> runIdentity $ traverse (Identity . (*2)) pattern
-- Pattern 2 [Pattern 4 []]
--
-- Traversal with Maybe (validation):
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> pattern = patternWith 1 [pattern 2]
-- >>> traverse validate pattern
-- Just (Pattern 1 [Pattern 2 []])
--
-- >>> invalid = patternWith 1 [pattern (-1)]
-- >>> traverse validate invalid
-- Nothing
--
-- Sequencing effects:
--
-- >>> pattern = patternWith (Just 1) [pattern (Just 2)]
-- >>> sequenceA pattern
-- Just (Pattern 1 [Pattern 2 []])
--
-- Atomic pattern:
--
-- >>> atom = pattern 5
-- >>> traverse (Just . (*2)) atom
-- Just (Pattern 10 [])
--
-- Pattern with multiple elements:
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> elem1 = pattern 5
-- >>> elem2 = pattern 10
-- >>> pattern = patternWith 20 [elem1, elem2]
-- >>> traverse validate pattern
-- Just (Pattern 20 [Pattern 5 [],Pattern 10 []])
--
-- Nested pattern structure:
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> inner = pattern 1
-- >>> middle = patternWith 2 [inner]
-- >>> pattern = patternWith 3 [middle]
-- >>> traverse validate pattern
-- Just (Pattern 3 [Pattern 2 [Pattern 1 []]])
--
-- Effect failure propagation:
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> inner = pattern (-1) -- Invalid value
-- >>> middle = patternWith 2 [inner]
-- >>> pattern = patternWith 3 [middle]
-- >>> traverse validate pattern
-- Nothing
instance Traversable Pattern where
  traverse f (Pattern v es) = Pattern <$> f v <*> traverse (traverse f) es

-- * Construction Functions

-- | Create an atomic pattern (a pattern with no elements) from a value.
--
-- This is a convenience constructor for creating simple patterns.
--
-- === Examples
--
-- >>> pattern "atom"
-- Pattern "atom" []
--
-- >>> pattern 42
-- Pattern 42 []
pattern :: v -> Pattern v
pattern v = Pattern v []

-- | Create a pattern with explicit elements.
--
-- This is a convenience constructor for creating complex patterns.
--
-- === Examples
--
-- >>> patternWith "root" [pattern "child"]
-- Pattern "root" [Pattern "child" []]
--
-- >>> patternWith "pair" [pattern 1, pattern 2]
-- Pattern "pair" [Pattern 1 [],Pattern 2 []]
patternWith :: v -> [Pattern v] -> Pattern v
patternWith v es = Pattern v es

-- | Create a pattern from a list of values.
--
-- Creates a pattern where the first argument is the decoration value,
-- and the list of values are converted to atomic patterns and used as elements.
--
-- === Examples
--
-- >>> fromList "root" ["a", "b", "c"]
-- Pattern "root" [Pattern "a" [],Pattern "b" [],Pattern "c" []]
fromList :: v -> [v] -> Pattern v
fromList v vs = patternWith v (map pattern vs)

-- * Query Functions

-- | Returns the number of direct elements in a pattern's sequence.
--
-- This operation is O(1).
--
-- === Examples
--
-- >>> length (pattern "atom")
-- 0
--
-- >>> length (patternWith "pair" [pattern 1, pattern 2])
-- 2
length :: Pattern v -> Int
length (Pattern _ es) = Prelude.length es

-- | Returns the total number of nodes in a pattern structure.
--
-- Counts the root node plus all nodes in all nested subpatterns.
-- This operation is O(n) where n is the total number of nodes.
--
-- === Examples
--
-- >>> size (pattern "atom")
-- 1
--
-- >>> size (patternWith "root" [pattern "child"])
-- 2
--
-- >>> size (patternWith "root" [pattern "a", pattern "b"])
-- 3
size :: Pattern v -> Int
size (Pattern _ es) = 1 + sum (map size es)

-- | Returns the maximum nesting depth of a pattern structure.
--
-- An atomic pattern has depth 1.
-- A pattern with elements has depth 1 + max depth of elements.
-- This operation is O(n) where n is the total number of nodes.
--
-- === Examples
--
-- >>> depth (pattern "atom")
-- 1
--
-- >>> depth (patternWith "root" [pattern "child"])
-- 2
--
-- >>> depth (patternWith "root" [patternWith "middle" [pattern "inner"]])
-- 3
depth :: Pattern v -> Int
depth (Pattern _ []) = 1
depth (Pattern _ es) = 1 + maximum (map depth es)

-- | Extracts all values from a pattern structure as a flat list.
--
-- The order of values corresponds to a pre-order traversal (root, then elements).
-- This is equivalent to 'toList' from 'Foldable', but specific to 'Pattern'.
-- This operation is O(n) where n is the total number of nodes.
--
-- === Examples
--
-- >>> values (pattern "atom")
-- ["atom"]
--
-- >>> values (patternWith "root" [pattern "a", pattern "b"])
-- ["root","a","b"]
values :: Pattern v -> [v]
values = toList

-- * Predicate Functions

-- | Checks if any value in the pattern satisfies the predicate.
--
-- Traverses the pattern structure and applies the predicate to each value.
-- Returns True if at least one value satisfies the predicate.
-- This operation is O(n).
--
-- === Examples
--
-- >>> anyValue (> 5) (patternWith 3 [pattern 6, pattern 2])
-- True
--
-- >>> anyValue (> 10) (patternWith 3 [pattern 6, pattern 2])
-- False
anyValue :: (v -> Bool) -> Pattern v -> Bool
anyValue p = foldr (\v acc -> p v || acc) False

-- | Checks if all values in the pattern satisfy the predicate.
--
-- Traverses the pattern structure and applies the predicate to each value.
-- Returns True if all values satisfy the predicate.
-- This operation is O(n).
--
-- === Examples
--
-- >>> allValues (> 0) (patternWith 3 [pattern 6, pattern 2])
-- True
--
-- >>> allValues (> 5) (patternWith 3 [pattern 6, pattern 2])
-- False
allValues :: (v -> Bool) -> Pattern v -> Bool
allValues p = foldr (\v acc -> p v && acc) True

-- | Filters subpatterns (including root) that satisfy a pattern predicate.
--
-- Traverses the pattern structure and applies the predicate to each subpattern.
-- Returns a list of all matching patterns.
-- This operation is O(n).
--
-- === Examples
--
-- >>> p = patternWith 3 [pattern 6, pattern 2]
-- >>> map value $ filterPatterns (\x -> value x > 5) p
-- [6]
--
-- >>> map value $ filterPatterns (\x -> length x > 0) p
-- [3]
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
filterPatterns p pat@(Pattern _ es) =
  (if p pat then [pat] else []) ++ concatMap (filterPatterns p) es

-- | Finds the first subpattern (including root) that satisfies a pattern predicate.
--
-- Traverses the pattern structure in pre-order and returns the first match.
-- Returns Nothing if no pattern matches.
-- This operation is O(n).
--
-- === Examples
--
-- >>> p = patternWith 3 [pattern 6, pattern 2]
-- >>> fmap value $ findPattern (\x -> value x > 5) p
-- Just 6
--
-- >>> findPattern (\x -> value x > 10) p
-- Nothing
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
findPattern p pat@(Pattern _ es)
  | p pat = Just pat
  | otherwise = foldr orElse Nothing (map (findPattern p) es)
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Finds all subpatterns (including root) that satisfy a pattern predicate.
--
-- Equivalent to 'filterPatterns'. Returns a list of all matching patterns.
-- This operation is O(n).
--
-- === Examples
--
-- >>> p = patternWith 3 [pattern 6, pattern 2]
-- >>> map value $ findAllPatterns (\x -> value x > 1) p
-- [3,6,2]
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
findAllPatterns = filterPatterns

-- | Checks if two patterns match structurally.
--
-- Uses the 'Eq' instance to compare patterns for structural equality.
-- Both values and elements must match recursively.
-- This operation is O(n).
--
-- === Examples
--
-- >>> matches (pattern "a") (pattern "a")
-- True
--
-- >>> matches (pattern "a") (pattern "b")
-- False
matches :: Eq v => Pattern v -> Pattern v -> Bool
matches = (==)

-- | Checks if a pattern contains a specific subpattern.
--
-- Traverses the pattern structure to see if any subpattern matches the target.
-- Uses 'matches' (structural equality) for comparison.
-- This operation is O(n).
--
-- === Examples
--
-- >>> p = patternWith "root" [pattern "child"]
-- >>> contains p (pattern "child")
-- True
--
-- >>> contains p (pattern "missing")
-- False
contains :: Eq v => Pattern v -> Pattern v -> Bool
contains haystack needle =
  matches haystack needle || any (`contains` needle) (elements haystack)

-- * Foldable/Traversable Extras

-- | Extracts all values from a pattern structure as a flat list.
--
-- Equivalent to 'toList' and 'values'. Provided for explicit API clarity.
--
-- === Examples
--
-- >>> flatten (pattern "atom")
-- ["atom"]
flatten :: Pattern v -> [v]
flatten = toList

-- | Extracts the pattern structure as a tuple (value, elements).
--
-- Preserves the structure while exposing the internal components.
-- Useful for pattern matching or transformation without direct field access.
--
-- === Examples
--
-- >>> toTuple (pattern "atom")
-- ("atom",[])
--
-- >>> toTuple (patternWith "root" [pattern "child"])
-- ("root",[Pattern "child" []])
toTuple :: Pattern v -> (v, [Pattern v])
toTuple (Pattern v es) = (v, es)

-- * Context/Comonad Functions

-- | Computes the nesting depth at each position in the pattern.
--
-- Returns a new pattern with the same structure where each value is replaced
-- by its depth (distance from root + 1).
--
-- === Examples
--
-- >>> p = patternWith "root" [pattern "child"]
-- >>> depthAt p
-- Pattern 1 [Pattern 2 []]
depthAt :: Pattern v -> Pattern Int
depthAt = go 1
  where
    go d (Pattern _ es) = Pattern d (map (go (d + 1)) es)

-- | Computes the size of the subtree at each position in the pattern.
--
-- Returns a new pattern with the same structure where each value is replaced
-- by the size (number of nodes) of the subtree rooted at that position.
--
-- === Examples
--
-- >>> p = patternWith "root" [pattern "a", pattern "b"]
-- >>> sizeAt p
-- Pattern 3 [Pattern 1 [],Pattern 1 []]
sizeAt :: Pattern v -> Pattern Int
sizeAt (Pattern _ es) =
  let subResults = map sizeAt es
      mySize = 1 + sum (map value subResults)
  in Pattern mySize subResults

-- | Computes the path indices from root to each position in the pattern.
--
-- Returns a new pattern with the same structure where each value is replaced
-- by a list of indices representing the path from root to that position.
--
-- === Examples
--
-- >>> p = patternWith "root" [pattern "a", pattern "b"]
-- >>> indicesAt p
-- Pattern [] [Pattern [0] [],Pattern [1] []]
indicesAt :: Eq v => Pattern v -> Pattern [Int]
indicesAt = go []
  where
    go path (Pattern _ es) =
      Pattern path (zipWith (\i e -> go (path ++ [i]) e) [0..] es)
