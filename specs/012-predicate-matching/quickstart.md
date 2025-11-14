# Quickstart: Predicate-Based Pattern Matching

**Feature**: 012-predicate-matching  
**Date**: 2025-01-28  
**Status**: Design Complete

## Overview

Predicate-based pattern matching enables flexible querying and filtering of patterns based on value properties and structural characteristics. The feature includes three categories of functions: (1) value predicate functions that check if **flattened values** in patterns satisfy predicates, (2) pattern predicate functions that find and filter subpatterns matching **structural criteria** (including element sequence patterns), and (3) structural matching functions that perform structural pattern matching beyond exact equality.

**Key Distinction**:
- **Value predicates** operate on **flattened values** extracted from all nesting levels (via `Foldable.toList`)
- **Pattern predicates** operate on **pattern structures** and can match on element sequences, repetition patterns, structural arrangements, etc., regardless of the depth or content of individual elements

## Basic Usage

### Value Predicate Functions

#### anyValue

Check if any value in a pattern satisfies a predicate:

```haskell
import Pattern.Core

-- Check if pattern contains any positive values
let pat = patternWith 0 [pattern 1, pattern 2]
anyValue (> 0) pat  -- True (values 1 and 2 are positive)

-- Check if pattern contains any negative values
anyValue (< 0) pat  -- False (no negative values)

-- Check atomic pattern
anyValue (> 0) (pattern 1)  -- True
anyValue (> 0) (pattern 0)  -- False
```

#### allValues

Check if all values in a pattern satisfy a predicate:

```haskell
-- Check if all values are positive
let pat = patternWith 1 [pattern 2, pattern 3]
allValues (> 0) pat  -- True (all values are positive)

-- Check if all values are greater than 1
allValues (> 1) pat  -- False (value 1 is not > 1)

-- Vacuous truth for empty patterns
allValues (> 0) (pattern 0)  -- False (value 0 doesn't match)
```

### Pattern Predicate Functions

#### filterPatterns

Filter all subpatterns that match a predicate:

```haskell
-- Find all atomic patterns (patterns with no elements)
let pat = patternWith "root" [pattern "a", pattern "b", patternWith "c" [pattern "d"]]
filterPatterns (\p -> length (elements p) == 0) pat
-- Result: [pattern "a", pattern "b", pattern "d"]

-- Find all patterns with specific value
filterPatterns (\p -> value p == "c") pat
-- Result: [patternWith "c" [pattern "d"]]

-- Find all patterns with depth > 1
filterPatterns (\p -> depth p > 1) pat
-- Result: [patternWith "c" [pattern "d"]]
```

#### findPattern

Find the first subpattern that matches a predicate:

```haskell
-- Find first atomic pattern
let pat = patternWith "root" [pattern "a", pattern "b"]
findPattern (\p -> length (elements p) == 0) pat
-- Result: Just (pattern "a")

-- Find pattern with specific value
findPattern (\p -> value p == "b") pat
-- Result: Just (pattern "b")

-- No match found
findPattern (\p -> value p == "x") pat
-- Result: Nothing
```

#### findAllPatterns

Find all subpatterns that match a predicate (same as `filterPatterns`):

```haskell
let pat = patternWith "root" [pattern "a", pattern "b"]
findAllPatterns (\p -> length (elements p) == 0) pat
-- Result: [pattern "a", pattern "b"]
```

### Structural Matching Functions

#### matches

Check if two patterns match structurally:

```haskell
-- Identical patterns match
let p1 = patternWith "root" [pattern "a", pattern "b"]
let p2 = patternWith "root" [pattern "a", pattern "b"]
matches p1 p2  -- True

-- Different values don't match
let p3 = patternWith "x" [pattern "a", pattern "b"]
matches p1 p3  -- False

-- Different element counts don't match
let p4 = patternWith "root" [pattern "a"]
matches p1 p4  -- False

-- Patterns with same flattened values but different structures don't match
let p5 = patternWith "root" [pattern "a"]
let p6 = patternWith "root" [patternWith "a" []]
matches p5 p6  -- False (different structures)
```

#### contains

Check if one pattern contains another as a subpattern:

```haskell
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]

-- Contains atomic subpattern
contains pat (pattern "a")  -- True

-- Contains nested subpattern
contains pat (patternWith "b" [pattern "c"])  -- True

-- Doesn't contain non-existent subpattern
contains pat (pattern "x")  -- False

-- Pattern contains itself
contains pat pat  -- True
```

## Common Use Cases

### Use Case 1: Validation

Validate that all values in a pattern satisfy certain conditions:

```haskell
-- Check if all values are positive
let pat = patternWith 1 [pattern 2, pattern 3]
if allValues (> 0) pat
  then "All values are positive"
  else "Some values are not positive"

-- Check if pattern contains any invalid values
if anyValue (< 0) pat
  then "Pattern contains invalid values"
  else "All values are valid"
```

### Use Case 2: Pattern Filtering

Filter patterns based on structural properties:

```haskell
-- Find all leaf patterns (atomic patterns)
let pat = patternWith "root" [pattern "a", pattern "b", patternWith "c" [pattern "d"]]
let leaves = filterPatterns (\p -> length (elements p) == 0) pat
-- Result: [pattern "a", pattern "b", pattern "d"]

-- Find all patterns with specific depth
let deepPatterns = filterPatterns (\p -> depth p > 2) pat
-- Result: Patterns with depth > 2

-- Find all patterns with specific size
let largePatterns = filterPatterns (\p -> size p > 10) pat
-- Result: Patterns with more than 10 nodes
```

### Use Case 2b: Matching on Element Sequence Structure

Find patterns based on structural patterns in their elements list (e.g., repetition, palindromes):

```haskell
-- Find patterns where elements form a palindrome-like sequence (a, b, b, a)
let pat = patternWith "root" [pattern "a", pattern "b", pattern "b", pattern "a"]
filterPatterns (\p -> elements p == reverse (elements p)) pat
-- Result: [pat] (pattern where elements list is a palindrome)

-- Find patterns with repeating element pattern (a, b, b, a) regardless of a/b content
filterPatterns (\p -> length (elements p) == 4 && 
                     value (elements p !! 0) == value (elements p !! 3) &&
                     value (elements p !! 1) == value (elements p !! 2)) pat
-- Result: [pat] (pattern with a, b, b, a element pattern)

-- Find patterns where first and last elements match
filterPatterns (\p -> length (elements p) > 0 && 
                     value (head (elements p)) == value (last (elements p))) pat
-- Result: [pat] (pattern where first and last elements have same value)

-- Find patterns with specific element sequence regardless of element depth/content
let pat2 = patternWith "root" [patternWith "a" [pattern "x"], 
                                patternWith "b" [pattern "y"], 
                                patternWith "b" [pattern "z"], 
                                patternWith "a" [pattern "w"]]
filterPatterns (\p -> length (elements p) == 4 && 
                     value (elements p !! 0) == value (elements p !! 3) &&
                     value (elements p !! 1) == value (elements p !! 2)) pat2
-- Result: [pat2] (matches a, b, b, a pattern even though a and b are nested patterns)
```

### Use Case 3: Pattern Search

Search for specific patterns within a larger structure:

```haskell
-- Find first pattern matching a condition
let pat = patternWith "root" [pattern "a", pattern "b", pattern "c"]
case findPattern (\p -> value p == "b") pat of
  Just found -> "Found pattern: " ++ show found
  Nothing -> "Pattern not found"

-- Find all patterns matching a condition
let matches = findAllPatterns (\p -> value p `elem` ["a", "b"]) pat
-- Result: [pattern "a", pattern "b"]
```

### Use Case 4: Structural Comparison

Compare patterns based on structure:

```haskell
-- Check if two patterns have the same structure
let p1 = patternWith "root" [pattern "a", pattern "b"]
let p2 = patternWith "root" [pattern "a", pattern "b"]
if matches p1 p2
  then "Patterns have identical structure"
  else "Patterns have different structures"

-- Check if a pattern contains a specific subpattern
let pat = patternWith "root" [pattern "a", patternWith "b" [pattern "c"]]
let subpat = patternWith "b" [pattern "c"]
if contains pat subpat
  then "Pattern contains subpattern"
  else "Pattern does not contain subpattern"
```

### Use Case 5: Conditional Logic

Use predicates in conditional logic:

```haskell
-- Process pattern based on value properties
let pat = patternWith 5 [pattern 10, pattern 15]
if anyValue (> 20) pat
  then "Pattern contains large values"
  else if allValues (> 0) pat
    then "All values are positive"
    else "Some values are not positive"

-- Process pattern based on structure
let pat = patternWith "root" [pattern "a", pattern "b"]
case findPattern (\p -> depth p > 1) pat of
  Just deep -> "Found deep pattern: " ++ show deep
  Nothing -> "No deep patterns found"
```

## Advanced Examples

### Combining Predicates

Combine multiple predicates for complex queries:

```haskell
-- Find patterns that are atomic AND have specific value
let pat = patternWith "root" [pattern "a", pattern "b"]
filterPatterns (\p -> length (elements p) == 0 && value p == "a") pat
-- Result: [pattern "a"]

-- Find patterns with specific size AND depth
filterPatterns (\p -> size p > 5 && depth p > 2) pat
-- Result: Patterns matching both conditions
```

### Using with Existing Functions

Combine predicate functions with existing Pattern operations:

```haskell
-- Filter patterns and then extract their values
let pat = patternWith "root" [pattern "a", pattern "b", pattern "c"]
let filtered = filterPatterns (\p -> value p `elem` ["a", "b"]) pat
let values = map value filtered
-- Result: ["a", "b"]

-- Use predicates with Functor instance
let pat = patternWith 1 [pattern 2, pattern 3]
let transformed = fmap (* 2) pat
allValues (> 0) transformed  -- True (all values are positive after transformation)
```

### Performance Considerations

For large patterns, consider performance:

```haskell
-- anyValue short-circuits on first match (efficient)
anyValue (> 1000) largePattern  -- Stops as soon as first match found

-- allValues must check all values (no short-circuit)
allValues (> 0) largePattern  -- Checks all values

-- findPattern short-circuits on first match (efficient)
findPattern (\p -> size p > 100) largePattern  -- Stops as soon as first match found

-- filterPatterns must check all subpatterns (no short-circuit)
filterPatterns (\p -> size p > 100) largePattern  -- Checks all subpatterns
```

## Edge Cases

### Atomic Patterns

```haskell
-- Value predicates on atomic patterns
anyValue (> 0) (pattern 1)  -- True
allValues (> 0) (pattern 0)  -- False

-- Pattern predicates on atomic patterns
filterPatterns (\p -> value p == "a") (pattern "a")  -- [pattern "a"]
findPattern (\p -> value p == "a") (pattern "a")  -- Just (pattern "a")

-- Structural matching on atomic patterns
matches (pattern "a") (pattern "a")  -- True
matches (pattern "a") (pattern "b")  -- False

-- Containment on atomic patterns
contains (pattern "a") (pattern "a")  -- True (self-containment)
contains (pattern "a") (pattern "b")  -- False
```

### Empty Elements

```haskell
-- Value predicates on patterns with empty elements
anyValue (> 0) (patternWith 1 [])  -- True (value 1 matches)
allValues (> 0) (patternWith 0 [])  -- False (value 0 doesn't match)

-- Pattern predicates on patterns with empty elements
filterPatterns (\p -> length (elements p) == 0) (patternWith "a" [])  -- [patternWith "a" []]

-- Structural matching on patterns with empty elements
matches (patternWith "a" []) (patternWith "a" [])  -- True
```

### No Matches

```haskell
-- Value predicates with no matches
anyValue (< 0) (patternWith 1 [pattern 2, pattern 3])  -- False
allValues (> 10) (patternWith 1 [pattern 2, pattern 3])  -- False

-- Pattern predicates with no matches
filterPatterns (\p -> value p == "x") (patternWith "a" [pattern "b"])  -- []
findPattern (\p -> value p == "x") (patternWith "a" [pattern "b"])  -- Nothing

-- Structural matching with no matches
matches (pattern "a") (pattern "b")  -- False
contains (patternWith "a" [pattern "b"]) (pattern "x")  -- False
```

## Best Practices

1. **Use value predicates for value-based queries**: When you need to check **flattened value properties** (all values extracted from all nesting levels), use `anyValue` or `allValues`
2. **Use pattern predicates for structural queries**: When you need to find patterns based on **structure** (element sequences, repetition patterns, nesting patterns, etc.), use `filterPatterns`, `findPattern`, or `findAllPatterns`
3. **Pattern predicates can match on element sequences**: Pattern predicates receive the full `Pattern v` structure, so they can examine `elements p` to match on structural patterns like `a, b, b, a` regardless of the depth or content of individual elements
4. **Use structural matching for structure comparison**: When you need to compare patterns structurally, use `matches` or `contains`
5. **Consider performance**: Use `anyValue` and `findPattern` when you only need to know if a match exists (they short-circuit)
6. **Combine with existing functions**: Predicate functions work well with existing Pattern operations like `size`, `depth`, `values`

## Integration with Existing Features

Predicate functions integrate seamlessly with existing Pattern operations:

```haskell
-- Use with Functor instance
let pat = patternWith 1 [pattern 2, pattern 3]
let transformed = fmap (* 2) pat
allValues (> 0) transformed  -- True

-- Use with Foldable instance
let pat = patternWith 1 [pattern 2, pattern 3]
let allVals = toList pat  -- [1, 2, 3]
anyValue (> 2) pat  -- True (uses same value extraction)

-- Use with query functions
let pat = patternWith "root" [pattern "a", pattern "b"]
filterPatterns (\p -> size p > 1) pat  -- Uses size function
filterPatterns (\p -> depth p > 0) pat  -- Uses depth function
```

