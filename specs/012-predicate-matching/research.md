# Research: Predicate-Based Pattern Matching

**Feature**: 012-predicate-matching  
**Date**: 2025-01-28  
**Status**: Complete

## Research Objectives

1. Design value predicate functions (`anyValue`, `allValues`) that check if values in patterns satisfy predicates
2. Design pattern predicate functions (`filterPatterns`, `findPattern`, `findAllPatterns`) that find and filter subpatterns
3. Design structural matching functions (`matches`, `contains`) that perform structural pattern matching
4. Determine relationship with existing Foldable and Traversable instances
5. Review Haskell best practices for predicate functions and pattern matching

## Value Predicate Functions

### Decision: Implementation Strategy for `anyValue` and `allValues`

**Chosen Approach**: Use `Foldable` instance to extract all values, then apply predicates using standard list operations.

**Implementation Pattern**:
```haskell
anyValue :: (v -> Bool) -> Pattern v -> Bool
anyValue p = any p . toList

allValues :: (v -> Bool) -> Pattern v -> Bool
allValues p = all p . toList
```

**Rationale**:
- Leverages existing `Foldable` instance which already provides `toList` for extracting all values
- `Foldable` semantics ensure all values at all nesting levels are considered
- Standard Haskell pattern: `any` and `all` are standard list predicates
- Efficient: `Foldable` instance already optimized for value extraction
- Consistent with decorated sequence model: values are extracted independently of structure

**Alternatives Considered**:
- **Manual recursive traversal**: Rejected - duplicates `Foldable` functionality, violates DRY principle
- **Using `foldMap` with `Any`/`All` monoids**: Considered but rejected - `toList` + `any`/`all` is clearer and more direct
- **Early termination optimization**: Considered for `anyValue` (short-circuit on first match) but `Foldable.toList` already provides efficient traversal

### Decision: Relationship with Foldable Semantics

**Chosen Approach**: Value predicates operate on values extracted via `Foldable` semantics, treating values independently of structural context.

**Rationale**:
- Aligns with `Foldable` semantics: values are extracted and processed independently
- Consistent with existing `values` function which uses `toList`
- Matches user expectations: "does this pattern contain any negative numbers?" operates on values, not structure
- Enables efficient implementation using existing `Foldable` instance

**Verification**:
- `anyValue p pat` checks if any value in `pat` (at any nesting level) satisfies `p`
- `allValues p pat` checks if all values in `pat` (at any nesting level) satisfy `p`
- Both functions consider all values extracted via `toList`, which includes pattern's own value and all element values recursively

## Pattern Predicate Functions

### Decision: Implementation Strategy for `filterPatterns`, `findPattern`, `findAllPatterns`

**Chosen Approach**: Recursive traversal that includes root pattern and all nested subpatterns, collecting those that match the predicate.

**Implementation Pattern**:
```haskell
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
filterPatterns p pat = 
  let root = if p pat then [pat] else []
      nested = concatMap (filterPatterns p) (elements pat)
  in root ++ nested

findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
findPattern p pat
  | p pat = Just pat
  | otherwise = foldr (\e acc -> case acc of
      Nothing -> findPattern p e
      Just _ -> acc) Nothing (elements pat)

findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
findAllPatterns = filterPatterns  -- Same implementation
```

**Rationale**:
- Recursive traversal naturally handles nested structures
- Includes root pattern in search scope (as specified in requirements)
- `filterPatterns` and `findAllPatterns` are identical (both return all matches)
- `findPattern` uses short-circuit evaluation (returns first match)
- Standard Haskell pattern for tree traversal with predicate matching

**Alternatives Considered**:
- **Using `Foldable` or `Traversable`**: Rejected - these operate on values, not pattern structures
- **Breadth-first traversal**: Considered but rejected - depth-first is simpler and sufficient for requirements
- **Separate root/nested handling**: Considered but rejected - unified recursive approach is cleaner

### Decision: Search Scope (Root Pattern Inclusion)

**Chosen Approach**: Pattern predicate functions include the root pattern in their search scope, not just nested subpatterns.

**Rationale**:
- Specified in requirements: "including root pattern and all nested subpatterns"
- Matches user expectations: "find all patterns matching predicate" should include the pattern itself if it matches
- Consistent with structural matching semantics: root pattern is part of the structure
- Enables queries like "find all patterns with depth > 2" to include root if it matches

**Verification**:
- `filterPatterns p pat` includes `pat` in results if `p pat` is `True`
- `findPattern p pat` checks `pat` first before searching nested subpatterns
- `findAllPatterns p pat` includes `pat` in results if `p pat` is `True`

## Structural Matching Functions

### Decision: Implementation Strategy for `matches`

**Chosen Approach**: Recursive structural comparison that checks value equality and element structure recursively.

**Implementation Pattern**:
```haskell
matches :: (Eq v) => Pattern v -> Pattern v -> Bool
matches (Pattern v1 els1) (Pattern v2 els2) =
  v1 == v2 && length els1 == length els2 && 
  all (uncurry matches) (zip els1 els2)
```

**Rationale**:
- Structural matching requires exact structural correspondence (value and elements recursively)
- Distinguishes patterns with same flattened values but different structures
- Recursive comparison naturally handles nested structures
- Uses `Eq v` constraint for value comparison
- Standard pattern for structural equality in recursive types

**Alternatives Considered**:
- **Using `Eq` instance**: Rejected - `Eq` already provides exact equality, but `matches` is explicitly for structural matching (may have different semantics in future)
- **Flattened value comparison**: Rejected - would not distinguish patterns with different structures but same flattened values
- **Partial matching**: Considered but rejected - requirements specify exact structural matching

### Decision: Implementation Strategy for `contains`

**Chosen Approach**: Recursive search that checks if a pattern appears anywhere in another pattern's structure (including as root).

**Implementation Pattern**:
```haskell
contains :: (Eq v) => Pattern v -> Pattern v -> Bool
contains pat subpat =
  pat `matches` subpat || any (contains subpat) (elements pat)
```

**Rationale**:
- Checks if `subpat` matches root pattern first (self-containment)
- Recursively searches all nested subpatterns
- Uses `matches` for structural comparison
- Standard pattern for subpattern containment in tree structures

**Alternatives Considered**:
- **Using `Eq` for containment**: Considered but rejected - `contains` should use structural matching (`matches`), not just value equality
- **Breadth-first search**: Considered but rejected - depth-first is simpler and sufficient
- **Early termination**: Already implemented - `any` short-circuits on first match

### Decision: Relationship with `Eq` Instance

**Chosen Approach**: Structural matching functions (`matches`, `contains`) are distinct from `Eq` instance, though they may have similar semantics initially.

**Rationale**:
- `Eq` provides exact equality: `p1 == p2` means identical structure and values
- `matches` provides structural matching: may have different semantics in future (e.g., partial matching)
- `contains` provides subpattern containment: checks if one pattern appears in another
- Separation allows future extension (e.g., partial matching, wildcards) without affecting `Eq`

**Verification**:
- `matches` performs structural comparison (value and elements recursively)
- `contains` checks if a pattern appears anywhere in another pattern's structure
- Both use `Eq v` constraint for value comparison, but operate on pattern structures

## Haskell Best Practices

### Predicate Function Naming

**Standard Pattern**: Use descriptive names that indicate the operation:
- `anyValue`, `allValues` - clearly indicate value-level predicates
- `filterPatterns`, `findPattern`, `findAllPatterns` - clearly indicate pattern-level operations
- `matches`, `contains` - clearly indicate structural matching operations

**Rationale**: 
- Follows standard Haskell naming conventions for predicate functions
- Names clearly indicate what they operate on (values vs patterns vs structures)
- Consistent with existing Pattern library naming (e.g., `values`, `size`, `depth`)

### Type Signatures

**Standard Pattern**: Explicit type signatures with clear constraints:
```haskell
anyValue :: (v -> Bool) -> Pattern v -> Bool
allValues :: (v -> Bool) -> Pattern v -> Bool
filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)
findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]
matches :: (Eq v) => Pattern v -> Pattern v -> Bool
contains :: (Eq v) => Pattern v -> Pattern v -> Bool
```

**Rationale**:
- Clear type signatures enable type inference and documentation
- Constraints (`Eq v`) are explicit where needed
- Return types (`Bool`, `Maybe (Pattern v)`, `[Pattern v]`) clearly indicate behavior

### Documentation Requirements

**Required**:
- Explain what each function matches (values, patterns, structures)
- Document traversal semantics (all nesting levels, root inclusion)
- Provide examples showing usage patterns
- Document edge case behavior (atomic patterns, empty elements, no matches)
- Explain relationship with existing instances (`Foldable`, `Eq`)

## Performance Considerations

### Time Complexity

- **Value predicates** (`anyValue`, `allValues`): O(n) where n is total number of nodes (via `Foldable.toList`)
- **Pattern predicates** (`filterPatterns`, `findPattern`, `findAllPatterns`): O(n) where n is total number of subpatterns
- **Structural matching** (`matches`): O(min(n, m)) where n and m are sizes of patterns being compared (short-circuits on first mismatch)
- **Containment** (`contains`): O(n*m) worst case where n is size of containing pattern and m is size of subpattern (may need to check all positions)

### Optimization Opportunities

- **Early termination**: `anyValue` and `findPattern` can short-circuit on first match (already handled by `any` and `Maybe` semantics)
- **Memoization**: For repeated containment checks, could memoize results (not needed for initial implementation)
- **Parallel traversal**: For very large patterns, could parallelize traversal (not needed for initial implementation, complexity not justified)

## Edge Case Handling

### Atomic Patterns

- **Value predicates**: Evaluate predicate on pattern's value (single value, no elements)
- **Pattern predicates**: Include atomic pattern in results if predicate matches
- **Structural matching**: Atomic patterns match if values are equal
- **Containment**: Atomic pattern contains itself, contains other atomic patterns if values match

### Empty Elements

- **Value predicates**: Consider pattern's value even if elements are empty
- **Pattern predicates**: Include pattern in results if predicate matches, even if elements are empty
- **Structural matching**: Patterns with empty elements match if values are equal
- **Containment**: Pattern with empty elements contains itself, contains atomic patterns if values match

### Deeply Nested Structures

- **All functions**: Recursive implementation naturally handles arbitrary nesting depth
- **Performance**: O(n) complexity ensures reasonable performance up to 100 levels (as specified)
- **Stack overflow**: Haskell's lazy evaluation and tail-call optimization handle deep recursion efficiently

### No Matches

- **Value predicates**: Return `False` for `anyValue`, `True` for `allValues` (vacuous truth)
- **Pattern predicates**: Return empty list for `filterPatterns`/`findAllPatterns`, `Nothing` for `findPattern`
- **Structural matching**: Return `False` if structures don't match
- **Containment**: Return `False` if subpattern doesn't appear

## Implementation Strategy

### Phase 1: Value Predicate Functions
- Implement `anyValue` and `allValues` using `Foldable.toList`
- Add comprehensive Haddock documentation
- Write unit tests for atomic patterns, nested patterns, edge cases

### Phase 2: Pattern Predicate Functions
- Implement `filterPatterns`, `findPattern`, `findAllPatterns` with recursive traversal
- Add comprehensive Haddock documentation
- Write unit tests for various structures, root inclusion, edge cases

### Phase 3: Structural Matching Functions
- Implement `matches` and `contains` with recursive structural comparison
- Add comprehensive Haddock documentation
- Write unit tests for structural matching, containment, edge cases

### Phase 4: Integration and Verification
- Write property-based tests for predicate function properties
- Test integration with existing Pattern operations and typeclass instances
- Verify performance targets (1000 nodes, 100 levels depth)

## Conclusion

**Decision**: ✅ Proceed with predicate-based pattern matching implementation

**Rationale**:
1. Value predicates can leverage existing `Foldable` instance for efficient implementation
2. Pattern predicates require recursive traversal (standard tree traversal pattern)
3. Structural matching functions provide distinct value beyond `Eq` instance
4. All functions have clear semantics and handle edge cases correctly
5. Implementation follows standard Haskell patterns and best practices
6. Performance targets are achievable with O(n) complexity
7. Functions naturally extend existing Pattern library operations

**Next Steps**: Proceed to Phase 1 design (data-model.md, contracts, quickstart.md)

