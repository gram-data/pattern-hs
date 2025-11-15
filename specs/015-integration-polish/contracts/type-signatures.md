# Type Signatures: Integration and Polish

**Feature**: Integration and Polish  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

This document defines the public API type signatures for the Pattern library. These signatures represent the contract that will be exported from Pattern.Core and re-exported through the main Pattern module.

## Core Type

### Pattern Type

```haskell
-- | A recursive structure representing a decorated sequence pattern.
data Pattern v = Pattern 
  { value :: v              -- Decoration value
  , elements :: [Pattern v] -- Sequence of pattern elements
  }
```

**Type Parameters**:
- `v` - The type of decoration values (must be consistent across all patterns in a structure)

**Constraints**: None (polymorphic in `v`)

## Construction Functions

### pattern

```haskell
-- | Create an atomic pattern (pattern with no elements) from a single value.
pattern :: v -> Pattern v
```

**Purpose**: Creates an atomic pattern with the given decoration value and empty elements list.

**Example**:
```haskell
atom = pattern "A"
-- Pattern {value = "A", elements = []}
```

### patternWith

```haskell
-- | Create a pattern with elements from a value and a list of pattern elements.
patternWith :: v -> [Pattern v] -> Pattern v
```

**Purpose**: Creates a pattern with the given decoration value and list of element patterns.

**Example**:
```haskell
p = patternWith "knows" [pattern "Alice", pattern "Bob"]
-- Pattern {value = "knows", elements = [Pattern {value = "Alice", elements = []}, Pattern {value = "Bob", elements = []}]}
```

### fromList

```haskell
-- | Create a pattern from a list of values by converting each value to an atomic pattern.
fromList :: v -> [v] -> Pattern v
```

**Purpose**: Creates a pattern from a decoration value and a list of values, converting each value to an atomic pattern.

**Example**:
```haskell
p = fromList "sequence" ["a", "b", "c"]
-- Pattern {value = "sequence", elements = [pattern "a", pattern "b", pattern "c"]}
```

## Query Functions

### length

```haskell
-- | Returns the number of direct elements in a pattern's sequence (O(1)).
length :: Pattern v -> Int
```

**Purpose**: Returns the count of direct elements in the pattern's elements list.

**Example**:
```haskell
length (patternWith "root" [pattern "a", pattern "b"])  -- 2
```

### size

```haskell
-- | Returns the total number of nodes in a pattern structure, including all nested patterns (O(n)).
size :: Pattern v -> Int
```

**Purpose**: Returns the total count of all nodes (patterns) in the structure, including nested patterns.

**Example**:
```haskell
size (patternWith "root" [pattern "a", pattern "b"])  -- 3 (root + 2 elements)
```

### depth

```haskell
-- | Returns the maximum nesting depth of a pattern structure (O(n)).
depth :: Pattern v -> Int
```

**Purpose**: Returns the maximum depth of nesting in the pattern structure.

**Example**:
```haskell
depth (pattern "a")  -- 0 (atomic pattern)
depth (patternWith "root" [pattern "a"])  -- 1
```

### values

```haskell
-- | Extracts all values from a pattern structure as a flat list (O(n)).
values :: Pattern v -> [v]
```

**Purpose**: Extracts all decoration values from the pattern structure as a flat list.

**Example**:
```haskell
values (patternWith "root" [pattern "a", pattern "b"])  -- ["root", "a", "b"]
```

### value

```haskell
-- | Access a pattern's decoration value (O(1), field accessor).
value :: Pattern v -> v
```

**Purpose**: Direct access to a pattern's decoration value.

**Example**:
```haskell
value (pattern "test")  -- "test"
```

## Predicate Functions

### anyValue

```haskell
-- | Checks if any value in a pattern satisfies a predicate (O(n)).
anyValue :: (v -> Bool) -> Pattern v -> Bool
```

**Purpose**: Checks if any decoration value in the pattern structure satisfies the given predicate.

**Example**:
```haskell
anyValue (> 5) (patternWith 10 [pattern 3, pattern 7])  -- True
```

### allValues

```haskell
-- | Checks if all values in a pattern satisfy a predicate (O(n)).
allValues :: (v -> Bool) -> Pattern v -> Bool
```

**Purpose**: Checks if all decoration values in the pattern structure satisfy the given predicate.

**Example**:
```haskell
allValues (> 0) (patternWith 10 [pattern 3, pattern 7])  -- True
```

### filterPatterns

```haskell
-- | Filters all subpatterns (including root) matching a pattern predicate (O(n)).
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Purpose**: Returns all subpatterns (including the root) that satisfy the given predicate.

**Example**:
```haskell
filterPatterns (\p -> length p > 1) (patternWith "root" [pattern "a", pattern "b"])  -- [root pattern]
```

### findPattern

```haskell
-- | Finds the first subpattern (including root) matching a pattern predicate (O(n)).
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
```

**Purpose**: Finds the first subpattern (including root) that satisfies the given predicate.

**Example**:
```haskell
findPattern (\p -> value p == "target") (patternWith "root" [pattern "target"])  -- Just (pattern "target")
```

### findAllPatterns

```haskell
-- | Finds all subpatterns (including root) matching a pattern predicate (O(n)).
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Purpose**: Equivalent to `filterPatterns`. Finds all subpatterns matching the predicate.

### matches

```haskell
-- | Checks if two patterns match structurally (O(n), equivalent to ==).
matches :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Purpose**: Checks if two patterns have the same structure and values (equivalent to `==`).

**Example**:
```haskell
matches (pattern "a") (pattern "a")  -- True
```

### contains

```haskell
-- | Checks if a pattern contains a subpattern (O(n)).
contains :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Purpose**: Checks if the first pattern contains the second pattern as a subpattern.

**Example**:
```haskell
contains (patternWith "root" [pattern "a"]) (pattern "a")  -- True
```

## Helper Functions

### flatten

```haskell
-- | Extracts all values as a flat list (equivalent to toList).
flatten :: Pattern a -> [a]
```

**Purpose**: Extracts all decoration values as a flat list (equivalent to `Foldable.toList`).

### toTuple

```haskell
-- | Extracts pattern as tuple preserving structure.
toTuple :: Pattern v -> (v, [Pattern v])
```

**Purpose**: Extracts a pattern as a tuple of its value and elements list.

## Comonad Helper Functions

### depthAt

```haskell
-- | Computes depth at each position.
depthAt :: Pattern v -> Pattern Int
```

**Purpose**: Creates a pattern where each position contains its depth in the structure.

### sizeAt

```haskell
-- | Computes size of subtree at each position.
sizeAt :: Pattern v -> Pattern Int
```

**Purpose**: Creates a pattern where each position contains the size of its subtree.

### indicesAt

```haskell
-- | Computes indices from root at each position.
indicesAt :: (Eq v) => Pattern v -> Pattern [Int]
```

**Purpose**: Creates a pattern where each position contains its path (list of indices) from the root.

## Typeclass Instances

### Show

```haskell
instance (Show v) => Show (Pattern v)
```

**Constraints**: Requires `Show v`

### Eq

```haskell
instance (Eq v) => Eq (Pattern v)
```

**Constraints**: Requires `Eq v`

### Functor

```haskell
instance Functor Pattern where
  fmap :: (a -> b) -> Pattern a -> Pattern b
```

**Laws**: Identity (`fmap id = id`), Composition (`fmap (f . g) = fmap f . fmap g`)

### Foldable

```haskell
instance Foldable Pattern where
  foldr :: (a -> b -> b) -> b -> Pattern a -> b
  foldl :: (a -> b -> a) -> b -> Pattern a -> b
  foldMap :: (Monoid m) => (a -> m) -> Pattern a -> m
  toList :: Pattern a -> [a]
```

### Traversable

```haskell
instance Traversable Pattern where
  traverse :: (Applicative f) => (a -> f b) -> Pattern a -> f (Pattern b)
  sequenceA :: (Applicative f) => Pattern (f a) -> f (Pattern a)
```

**Laws**: Naturality, Identity, Composition

### Ord

```haskell
instance (Ord v) => Ord (Pattern v) where
  compare :: Pattern v -> Pattern v -> Ordering
```

**Constraints**: Requires `Ord v`  
**Semantics**: Lexicographic ordering (value first, then elements recursively)

### Semigroup

```haskell
instance (Semigroup v) => Semigroup (Pattern v) where
  (<>) :: Pattern v -> Pattern v -> Pattern v
```

**Constraints**: Requires `Semigroup v`  
**Laws**: Associativity (`(a <> b) <> c = a <> (b <> c)`)

### Monoid

```haskell
instance (Monoid v) => Monoid (Pattern v) where
  mempty :: Pattern v
  mappend :: Pattern v -> Pattern v -> Pattern v  -- Uses <> from Semigroup
```

**Constraints**: Requires `Monoid v`  
**Laws**: Identity (`mempty <> a = a`, `a <> mempty = a`)

### Hashable

```haskell
instance (Hashable v) => Hashable (Pattern v) where
  hashWithSalt :: Int -> Pattern v -> Int
```

**Constraints**: Requires `Hashable v`  
**Properties**: Equal patterns produce equal hash values

### Applicative

```haskell
instance Applicative Pattern where
  pure :: a -> Pattern a
  (<*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
```

**Laws**: Identity, Composition, Homomorphism, Interchange

### Comonad

```haskell
instance Comonad Pattern where
  extract :: Pattern v -> v
  duplicate :: Pattern v -> Pattern (Pattern v)
  extend :: (Pattern v -> w) -> Pattern v -> Pattern w
```

**Laws**: Extract-Extend (`extract . extend f = f`), Extend-Extract (`extend extract = id`), Extend Composition (`extend f . extend g = extend (f . extend g)`)

## Notes

- All type signatures represent the public API contract
- Internal functions and implementation details are not included
- Type constraints are documented where applicable
- Mathematical laws are referenced but not fully specified here (see documentation)

