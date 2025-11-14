# Data Model: Predicate-Based Pattern Matching

**Feature**: 012-predicate-matching  
**Date**: 2025-01-28  
**Status**: Design Complete

## Overview

This document describes the predicate-based pattern matching functions for the `Pattern` type, enabling flexible querying and filtering of patterns based on value properties and structural characteristics. The feature includes three categories of functions: (1) value predicate functions that check if **flattened values** in patterns satisfy predicates (operate on values extracted via `Foldable.toList`), (2) pattern predicate functions that find and filter subpatterns matching **structural criteria** (operate on pattern structures including element sequences), and (3) structural matching functions that perform structural pattern matching beyond exact equality.

**Key Distinction**: 
- **Value predicates** (`anyValue`, `allValues`) operate on **flattened values** - all values extracted from the pattern structure regardless of nesting level
- **Pattern predicates** (`filterPatterns`, `findPattern`, `findAllPatterns`) operate on **pattern structures** - can examine the structure of elements lists, element sequences, nesting patterns, etc., not just flattened values

## Core Entities

### Pattern Type

The `Pattern` type is a recursive data structure representing decorated sequences:

```haskell
data Pattern v = Pattern 
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]    -- The pattern itself, as a sequence of elements
  }
  deriving (Eq)
```

**Existing Properties**:
- Has `Eq` instance for equality comparison
- Has `Show` instance for display
- Has `Functor`, `Foldable`, and `Traversable` instances
- Has `Ord` instance for ordering
- Has `Semigroup` and `Monoid` instances for pattern combination
- Recursive structure enables arbitrary nesting

### Value Predicate

A function that takes a value and returns a boolean, used to test individual values within patterns:

```haskell
type ValuePredicate v = v -> Bool
```

**Semantics**:
- Operates on individual values extracted from patterns
- Values are extracted independently of structural context (via `Foldable` semantics)
- Applied to all values at all nesting levels in the pattern structure

### Pattern Predicate

A function that takes a pattern and returns a boolean, used to test entire pattern structures:

```haskell
type PatternPredicate v = Pattern v -> Bool
```

**Semantics**:
- Operates on entire pattern structures (including value and elements)
- Can examine **structural properties** of the pattern, including:
  - The structure of the `elements` list (sequence, repetition, patterns in element order)
  - Nesting depth and structure
  - Element count and arrangement
  - Relationships between elements (not just their flattened values)
- Can consider both structure and values
- Applied to root pattern and all nested subpatterns at all depths

**Important**: Pattern predicates operate on **pattern structures**, not flattened values. They can match on structural patterns in the elements list (e.g., finding patterns where elements repeat like `a, b, b, a`), regardless of the depth or content of individual elements.

### Subpattern

Any pattern that appears within another pattern's structure, including the root pattern itself and all nested patterns at any depth.

**Semantics**:
- Root pattern is considered a subpattern of itself
- All patterns in the `elements` list are subpatterns
- All patterns nested within element patterns are subpatterns (recursively)

## Value Predicate Functions

### anyValue

**Type Signature**:
```haskell
anyValue :: (v -> Bool) -> Pattern v -> Bool
```

**Semantics**:
- Checks if any value in the pattern (at any nesting level) satisfies the predicate
- Returns `True` if at least one value matches, `False` otherwise
- Considers all values extracted via `Foldable.toList` (pattern's own value and all element values recursively)
- Short-circuits on first match (via `any` function)

**Implementation**:
```haskell
anyValue p = any p . toList
```

**Example**:
```haskell
let pat = patternWith "root" [pattern "a", pattern "b"]
anyValue (== "a") pat  -- True (value "a" matches)
anyValue (> 0) (patternWith 0 [pattern 1, pattern 2])  -- True (values 1 and 2 match)
anyValue (< 0) (patternWith 0 [pattern 1, pattern 2])  -- False (no negative values)
```

### allValues

**Type Signature**:
```haskell
allValues :: (v -> Bool) -> Pattern v -> Bool
```

**Semantics**:
- Checks if all values in the pattern (at any nesting level) satisfy the predicate
- Returns `True` only if every value matches, `False` otherwise
- Considers all values extracted via `Foldable.toList` (pattern's own value and all element values recursively)
- Returns `True` for empty patterns (vacuous truth)

**Implementation**:
```haskell
allValues p = all p . toList
```

**Example**:
```haskell
let pat = patternWith 1 [pattern 2, pattern 3]
allValues (> 0) pat  -- True (all values are positive)
allValues (> 1) pat  -- False (value 1 doesn't match)
allValues (> 0) (pattern 0)  -- False (value 0 doesn't match)
```

## Pattern Predicate Functions

### filterPatterns

**Type Signature**:
```haskell
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Semantics**:
- Filters all subpatterns (including root) that match the predicate
- Returns a list of all matching subpatterns
- Includes root pattern in results if it matches
- Includes all nested subpatterns at all depths that match
- May include duplicates if a pattern appears multiple times in the structure

**Implementation**:
```haskell
filterPatterns p pat = 
  let root = if p pat then [pat] else []
      nested = concatMap (filterPatterns p) (elements pat)
  in root ++ nested
```

**Example**:
```haskell
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
filterPatterns (\p -> length (elements p) == 0) pat
-- Result: [pattern "a", pattern "c"] (atomic patterns)

filterPatterns (\p -> value p == "b") pat
-- Result: [patternWith "b" [pattern "c"]] (pattern with value "b")

-- Match on element sequence structure (e.g., finding patterns with repeating elements)
let pat2 = patternWith "root" [pattern "a", pattern "b", pattern "b", pattern "a"]
filterPatterns (\p -> elements p == reverse (elements p)) pat2
-- Result: [pat2] (pattern where elements form a palindrome-like sequence)

-- Match on element patterns regardless of their internal content
filterPatterns (\p -> length (elements p) == 4 && 
                     value (elements p !! 0) == value (elements p !! 3) &&
                     value (elements p !! 1) == value (elements p !! 2)) pat2
-- Result: [pat2] (pattern with a, b, b, a element pattern)
```

### findPattern

**Type Signature**:
```haskell
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
```

**Semantics**:
- Finds the first subpattern (including root) that matches the predicate
- Returns `Just` the first matching subpattern, or `Nothing` if none match
- Checks root pattern first, then searches nested subpatterns depth-first
- Short-circuits on first match (doesn't search remaining subpatterns)

**Implementation**:
```haskell
findPattern p pat
  | p pat = Just pat
  | otherwise = foldr (\e acc -> case acc of
      Nothing -> findPattern p e
      Just _ -> acc) Nothing (elements pat)
```

**Example**:
```haskell
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
findPattern (\p -> value p == "b") pat
-- Result: Just (patternWith "b" [pattern "c"])

findPattern (\p -> value p == "x") pat
-- Result: Nothing (no pattern with value "x")
```

### findAllPatterns

**Type Signature**:
```haskell
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Semantics**:
- Finds all subpatterns (including root) that match the predicate
- Returns a list of all matching subpatterns
- Identical to `filterPatterns` (same implementation)
- Includes root pattern in results if it matches
- Includes all nested subpatterns at all depths that match

**Implementation**:
```haskell
findAllPatterns = filterPatterns  -- Same implementation
```

**Example**:
```haskell
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
findAllPatterns (\p -> length (elements p) == 0) pat
-- Result: [pattern "a", pattern "c"] (all atomic patterns)
```

## Structural Matching Functions

### matches

**Type Signature**:
```haskell
matches :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Semantics**:
- Checks if two patterns match structurally (value and elements recursively)
- Returns `True` if patterns have matching structure, `False` otherwise
- Distinguishes patterns based on structure, not just flattened values
- Requires `Eq v` constraint for value comparison

**Implementation**:
```haskell
matches (Pattern v1 els1) (Pattern v2 els2) =
  v1 == v2 && length els1 == length els2 && 
  all (uncurry matches) (zip els1 els2)
```

**Example**:
```haskell
let p1 = patternWith "root" [pattern "a", pattern "b"]
let p2 = patternWith "root" [pattern "a", pattern "b"]
matches p1 p2  -- True (identical structure)

let p3 = patternWith "root" [pattern "a"]
matches p1 p3  -- False (different element counts)

let p4 = patternWith "x" [pattern "a", pattern "b"]
matches p1 p4  -- False (different values)
```

### contains

**Type Signature**:
```haskell
contains :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Semantics**:
- Checks if one pattern contains another as a subpattern anywhere in its structure
- Returns `True` if the second pattern appears anywhere in the first pattern's structure, `False` otherwise
- Includes root pattern (pattern contains itself)
- Uses `matches` for structural comparison
- Requires `Eq v` constraint for value comparison

**Implementation**:
```haskell
contains pat subpat =
  pat `matches` subpat || any (contains subpat) (elements pat)
```

**Example**:
```haskell
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
contains pat (pattern "a")  -- True (contains atomic pattern "a")
contains pat (patternWith "b" [pattern "c"])  -- True (contains subpattern)
contains pat (pattern "x")  -- False (doesn't contain "x")
contains pat pat  -- True (pattern contains itself)
```

## Edge Cases

### Atomic Patterns

**Value predicates**: Evaluate predicate on pattern's value (single value, no elements)
```haskell
anyValue (> 0) (pattern 1)  -- True
allValues (> 0) (pattern 0)  -- False
```

**Pattern predicates**: Include atomic pattern in results if predicate matches
```haskell
filterPatterns (\p -> value p == "a") (pattern "a")  -- [pattern "a"]
findPattern (\p -> value p == "a") (pattern "a")  -- Just (pattern "a")
```

**Structural matching**: Atomic patterns match if values are equal
```haskell
matches (pattern "a") (pattern "a")  -- True
matches (pattern "a") (pattern "b")  -- False
```

**Containment**: Atomic pattern contains itself, contains other atomic patterns if values match
```haskell
contains (pattern "a") (pattern "a")  -- True
contains (pattern "a") (pattern "b")  -- False
```

### Empty Elements

**Value predicates**: Consider pattern's value even if elements are empty
```haskell
anyValue (> 0) (patternWith 1 [])  -- True (value 1 matches)
allValues (> 0) (patternWith 0 [])  -- False (value 0 doesn't match)
```

**Pattern predicates**: Include pattern in results if predicate matches, even if elements are empty
```haskell
filterPatterns (\p -> length (elements p) == 0) (patternWith "a" [])  -- [patternWith "a" []]
```

**Structural matching**: Patterns with empty elements match if values are equal
```haskell
matches (patternWith "a" []) (patternWith "a" [])  -- True
```

**Containment**: Pattern with empty elements contains itself, contains atomic patterns if values match
```haskell
contains (patternWith "a" []) (patternWith "a" [])  -- True
contains (patternWith "a" []) (pattern "a")  -- False (different structures)
```

### Deeply Nested Structures

All functions handle arbitrary nesting depth through recursive implementation:
```haskell
-- 100 levels deep
let deep = foldl (\p _ -> patternWith "level" [p]) (pattern "base") [1..100]
anyValue (== "base") deep  -- True (finds value at deepest level)
findPattern (\p -> value p == "base") deep  -- Just (pattern "base")
```

### No Matches

**Value predicates**: Return `False` for `anyValue`, `True` for `allValues` (vacuous truth)
```haskell
anyValue (< 0) (patternWith 1 [pattern 2, pattern 3])  -- False
allValues (< 0) (pattern 0)  -- True (vacuous truth for empty pattern)
```

**Pattern predicates**: Return empty list for `filterPatterns`/`findAllPatterns`, `Nothing` for `findPattern`
```haskell
filterPatterns (\p -> value p == "x") (patternWith "a" [pattern "b"])  -- []
findPattern (\p -> value p == "x") (patternWith "a" [pattern "b"])  -- Nothing
```

**Structural matching**: Return `False` if structures don't match
```haskell
matches (pattern "a") (pattern "b")  -- False
```

**Containment**: Return `False` if subpattern doesn't appear
```haskell
contains (patternWith "a" [pattern "b"]) (pattern "x")  -- False
```

## Validation Rules

### Value Predicate Rules

1. **All values considered**: Value predicates must consider all values at all nesting levels
2. **Foldable semantics**: Values are extracted independently of structural context
3. **Atomic patterns**: Predicate is evaluated on pattern's value for atomic patterns
4. **No matches**: `anyValue` returns `False` if no values match, `allValues` returns `True` for empty patterns (vacuous truth)

### Pattern Predicate Rules

1. **Root inclusion**: Pattern predicates must include root pattern in search scope
2. **All depths**: All nested subpatterns at all depths must be considered
3. **No matches**: Return empty list or `Nothing` appropriately if no matches found
4. **Duplicates**: May include duplicates if a pattern appears multiple times

### Structural Matching Rules

1. **Exact structure**: `matches` requires exact structural correspondence (value and elements recursively)
2. **Structure distinction**: Must distinguish patterns with same flattened values but different structures
3. **Self-matching**: Pattern matches itself
4. **Containment**: Pattern contains itself, contains subpatterns that appear anywhere in structure

## Relationships

### Relationship to Foldable Instance

Value predicate functions (`anyValue`, `allValues`) leverage the `Foldable` instance:
- Use `toList` to extract all values from pattern structure
- Values are extracted independently of structural context
- Consistent with `Foldable` semantics where values are processed in sequence

### Relationship to Eq Instance

Structural matching functions (`matches`, `contains`) are distinct from `Eq` instance:
- `Eq` provides exact equality: `p1 == p2` means identical structure and values
- `matches` provides structural matching: may have different semantics in future (e.g., partial matching)
- `contains` provides subpattern containment: checks if one pattern appears in another
- Both use `Eq v` constraint for value comparison, but operate on pattern structures

### Relationship to Existing Query Functions

Predicate functions extend existing query functions (`length`, `size`, `depth`, `values`):
- `values` extracts all values (used internally by value predicates)
- `size` and `depth` can be used in pattern predicates (e.g., `filterPatterns (\p -> size p > 10)`)
- Predicate functions provide flexible querying beyond simple structural queries

## State Transitions

N/A - All predicate and matching functions are pure functions with no state.

## Notes

- All functions are pure and have no side effects
- Recursive implementation naturally handles arbitrary nesting depth
- Performance: O(n) for value/pattern predicates, O(n*m) for structural matching/containment
- Functions leverage existing `Foldable` instance where appropriate
- Pattern predicates include root pattern in search scope (as specified in requirements)
- Structural matching distinguishes patterns based on structure, not just flattened values

