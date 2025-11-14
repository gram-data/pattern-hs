# Type Signatures: Predicate-Based Pattern Matching

**Feature**: 012-predicate-matching  
**Date**: 2025-01-28  
**Status**: Design Complete

## Value Predicate Functions

### anyValue

```haskell
anyValue :: (v -> Bool) -> Pattern v -> Bool
```

**Requirement**: No type constraints (works with any value type `v`)

**Semantics**: Checks if any value in the pattern (at any nesting level) satisfies the predicate. Returns `True` if at least one value matches, `False` otherwise.

**Example**:
```haskell
anyValue (> 0) (patternWith 0 [pattern 1, pattern 2])  -- True
anyValue (< 0) (patternWith 0 [pattern 1, pattern 2])  -- False
```

### allValues

```haskell
allValues :: (v -> Bool) -> Pattern v -> Bool
```

**Requirement**: No type constraints (works with any value type `v`)

**Semantics**: Checks if all values in the pattern (at any nesting level) satisfy the predicate. Returns `True` only if every value matches, `False` otherwise. Returns `True` for empty patterns (vacuous truth).

**Example**:
```haskell
allValues (> 0) (patternWith 1 [pattern 2, pattern 3])  -- True
allValues (> 1) (patternWith 1 [pattern 2, pattern 3])  -- False
```

## Pattern Predicate Functions

### filterPatterns

```haskell
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Requirement**: No type constraints (works with any value type `v`)

**Semantics**: Filters all subpatterns (including root) that match the predicate. Returns a list of all matching subpatterns.

**Example**:
```haskell
filterPatterns (\p -> length (elements p) == 0) (patternWith "root" [pattern "a", pattern "b"])
-- Result: [pattern "a", pattern "b"]
```

### findPattern

```haskell
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
```

**Requirement**: No type constraints (works with any value type `v`)

**Semantics**: Finds the first subpattern (including root) that matches the predicate. Returns `Just` the first matching subpattern, or `Nothing` if none match.

**Example**:
```haskell
findPattern (\p -> value p == "b") (patternWith "root" [pattern "a", pattern "b"])
-- Result: Just (pattern "b")
```

### findAllPatterns

```haskell
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
```

**Requirement**: No type constraints (works with any value type `v`)

**Semantics**: Finds all subpatterns (including root) that match the predicate. Returns a list of all matching subpatterns. Identical to `filterPatterns`.

**Example**:
```haskell
findAllPatterns (\p -> length (elements p) == 0) (patternWith "root" [pattern "a", pattern "b"])
-- Result: [pattern "a", pattern "b"]
```

## Structural Matching Functions

### matches

```haskell
matches :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Requirement**: `Eq v` constraint (value type must have `Eq` instance)

**Semantics**: Checks if two patterns match structurally (value and elements recursively). Returns `True` if patterns have matching structure, `False` otherwise. Distinguishes patterns based on structure, not just flattened values.

**Example**:
```haskell
matches (patternWith "root" [pattern "a", pattern "b"]) 
        (patternWith "root" [pattern "a", pattern "b"])  -- True
matches (patternWith "root" [pattern "a"]) 
        (patternWith "root" [pattern "a", pattern "b"])  -- False
```

### contains

```haskell
contains :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Requirement**: `Eq v` constraint (value type must have `Eq` instance)

**Semantics**: Checks if one pattern contains another as a subpattern anywhere in its structure. Returns `True` if the second pattern appears anywhere in the first pattern's structure, `False` otherwise. Includes root pattern (pattern contains itself).

**Example**:
```haskell
contains (patternWith "root" [pattern "a", pattern "b"]) (pattern "a")  -- True
contains (patternWith "root" [pattern "a", pattern "b"]) (pattern "x")  -- False
contains (patternWith "root" [pattern "a"]) (patternWith "root" [pattern "a"])  -- True
```

## Test Contracts

### Unit Test Requirements

#### Value Predicate Functions

1. **anyValue - Basic Functionality**
   - Test: `anyValue (> 0)` on pattern with positive values
   - Expected: Returns `True`
   - Test: `anyValue (< 0)` on pattern with no negative values
   - Expected: Returns `False`
   - Test: `anyValue` on atomic pattern
   - Expected: Evaluates predicate on pattern's value

2. **allValues - Basic Functionality**
   - Test: `allValues (> 0)` on pattern with all positive values
   - Expected: Returns `True`
   - Test: `allValues (> 0)` on pattern with some non-positive values
   - Expected: Returns `False`
   - Test: `allValues` on atomic pattern
   - Expected: Evaluates predicate on pattern's value

3. **Value Predicates - Nested Patterns**
   - Test: `anyValue` on deeply nested patterns
   - Expected: Considers all values at all nesting levels
   - Test: `allValues` on deeply nested patterns
   - Expected: Considers all values at all nesting levels

4. **Value Predicates - Edge Cases**
   - Test: `anyValue` on pattern with no matches
   - Expected: Returns `False`
   - Test: `allValues` on empty pattern (atomic with no elements)
   - Expected: Returns `True` (vacuous truth)

#### Pattern Predicate Functions

1. **filterPatterns - Basic Functionality**
   - Test: `filterPatterns` with predicate matching some subpatterns
   - Expected: Returns list of matching subpatterns
   - Test: `filterPatterns` with predicate matching root pattern
   - Expected: Includes root pattern in results
   - Test: `filterPatterns` with predicate matching no subpatterns
   - Expected: Returns empty list

2. **findPattern - Basic Functionality**
   - Test: `findPattern` with predicate matching first subpattern
   - Expected: Returns `Just` first matching subpattern
   - Test: `findPattern` with predicate matching no subpatterns
   - Expected: Returns `Nothing`
   - Test: `findPattern` with predicate matching root pattern
   - Expected: Returns `Just` root pattern (checks root first)

3. **findAllPatterns - Basic Functionality**
   - Test: `findAllPatterns` with predicate matching multiple subpatterns
   - Expected: Returns list of all matching subpatterns
   - Test: `findAllPatterns` with predicate matching no subpatterns
   - Expected: Returns empty list

4. **Pattern Predicates - Nested Patterns**
   - Test: `filterPatterns` on deeply nested patterns
   - Expected: Considers all nested subpatterns at all depths
   - Test: `findPattern` on deeply nested patterns
   - Expected: Finds first matching subpattern at any depth

5. **Pattern Predicates - Edge Cases**
   - Test: `filterPatterns` on atomic pattern
   - Expected: Includes atomic pattern if predicate matches
   - Test: `filterPatterns` on pattern with empty elements
   - Expected: Includes root pattern if predicate matches

#### Structural Matching Functions

1. **matches - Basic Functionality**
   - Test: `matches` on identical patterns
   - Expected: Returns `True`
   - Test: `matches` on patterns with different values
   - Expected: Returns `False`
   - Test: `matches` on patterns with different element counts
   - Expected: Returns `False`
   - Test: `matches` on patterns with same flattened values but different structures
   - Expected: Returns `False` (distinguishes based on structure)

2. **contains - Basic Functionality**
   - Test: `contains` on pattern containing subpattern
   - Expected: Returns `True`
   - Test: `contains` on pattern not containing subpattern
   - Expected: Returns `False`
   - Test: `contains` on pattern containing itself
   - Expected: Returns `True` (self-containment)

3. **Structural Matching - Nested Patterns**
   - Test: `matches` on deeply nested patterns
   - Expected: Recursively compares all nesting levels
   - Test: `contains` on deeply nested patterns
   - Expected: Recursively searches all nesting levels

4. **Structural Matching - Edge Cases**
   - Test: `matches` on atomic patterns
   - Expected: Returns `True` if values are equal
   - Test: `contains` on atomic patterns
   - Expected: Returns `True` if values match (self-containment)
   - Test: `matches` on patterns with empty elements
   - Expected: Returns `True` if values are equal

### Property-Based Test Requirements

1. **Value Predicate Properties**
   - Property: `anyValue p = not (allValues (not . p))` for complementary predicates
   - Test: Generate random patterns and predicates, verify property
   - Property: `anyValue (const True) = True` for all patterns
   - Test: Verify property holds for all pattern structures
   - Property: `allValues (const False) = False` for non-empty patterns
   - Test: Verify property holds for all non-empty pattern structures

2. **Pattern Predicate Properties**
   - Property: `filterPatterns (const True)` returns all subpatterns (including root)
   - Test: Generate random patterns, verify all subpatterns returned
   - Property: `filterPatterns (const False)` returns empty list
   - Test: Verify property holds for all pattern structures
   - Property: `findPattern p` returns `Just` first match from `filterPatterns p`
   - Test: Verify property holds for all pattern structures

3. **Structural Matching Properties**
   - Property: `matches` is reflexive: `matches p p = True` for all patterns `p`
   - Test: Generate random patterns, verify reflexivity
   - Property: `matches` is symmetric: `matches p1 p2 = matches p2 p1`
   - Test: Generate random pattern pairs, verify symmetry
   - Property: `contains` is reflexive: `contains p p = True` for all patterns `p`
   - Test: Generate random patterns, verify reflexivity
   - Property: `contains` is transitive: if `contains p1 p2` and `contains p2 p3`, then `contains p1 p3`
   - Test: Generate random pattern triples, verify transitivity

### Integration Test Requirements

1. **Integration with Existing Functions**
   - Test: Use `size` and `depth` in pattern predicates
   - Expected: Functions work correctly together
   - Test: Use `values` to verify value predicate results
   - Expected: Results are consistent

2. **Integration with Typeclass Instances**
   - Test: Use value predicates with `Functor` instance
   - Expected: Functions work correctly with transformed patterns
   - Test: Use pattern predicates with `Foldable` instance
   - Expected: Functions work correctly together

3. **Edge Cases**
   - Test: All functions on patterns with 100+ nesting levels
   - Expected: Functions complete in reasonable time
   - Test: All functions on patterns with 1000+ nodes
   - Expected: Functions complete in reasonable time
   - Test: All functions on patterns with duplicate values
   - Expected: Functions handle duplicates correctly

## Test Coverage Requirements

- **Unit tests**: Cover all functions on atomic patterns, nested patterns, and edge cases
- **Property-based tests**: Verify predicate function properties, structural matching properties
- **Integration tests**: Verify functions work correctly with existing Pattern operations and typeclass instances
- **Coverage target**: 100% of all predicate and matching functions covered by tests

## Performance Requirements

- **Value predicates** (`anyValue`, `allValues`): O(n) time where n is total number of nodes (via `Foldable.toList`)
- **Pattern predicates** (`filterPatterns`, `findPattern`, `findAllPatterns`): O(n) time where n is total number of subpatterns
- **Structural matching** (`matches`): O(min(n, m)) time where n and m are sizes of patterns being compared (short-circuits on first mismatch)
- **Containment** (`contains`): O(n*m) worst case where n is size of containing pattern and m is size of subpattern (may need to check all positions)
- **Performance targets**: All operations should complete in reasonable time for patterns with up to 1000 nodes and nesting depth up to 100 levels

