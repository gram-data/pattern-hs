# Type Signatures: Semigroup Instance for Pattern

**Feature**: 010-semigroup-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Semigroup Instance

### Type Signature

```haskell
instance Semigroup v => Semigroup (Pattern v) where
  (<>) :: Pattern v -> Pattern v -> Pattern v
```

### Implementation

```haskell
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)
```

### Type Constraints

- **Required**: `Semigroup v` - The value type must have a `Semigroup` instance
- **Provided**: `Semigroup (Pattern v)` - Patterns are combinable when values are combinable

### Semantics

- **Value combination**: `value (p1 <> p2) = value p1 <> value p2` (using value type's Semigroup)
- **Element concatenation**: `elements (p1 <> p2) = elements p1 ++ elements p2` (preserving order)
- **Associative**: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)` (Semigroup law)
- **Structure-preserving**: Preserves decorated sequence model (elements form pattern, value is decoration)

## Standard Semigroup Combinators

### sconcat

```haskell
-- Type signature
sconcat :: NonEmpty (Pattern v) -> Pattern v
```

**Requirement**: `Semigroup (Pattern v)` instance (satisfied when `Semigroup v`)

**Example**:
```haskell
sconcat (pattern "a" :| [pattern "b", pattern "c"])
-- Result: pattern "abc" (for String values) with concatenated elements
```

### stimes

```haskell
-- Type signature
stimes :: Integral n => n -> Pattern v -> Pattern v
```

**Requirement**: `Semigroup (Pattern v)` instance (satisfied when `Semigroup v`)

**Example**:
```haskell
stimes 3 (pattern "a")
-- Result: pattern "aaa" (for String values) with repeated elements
```

## Test Contracts

### Unit Test Requirements

1. **Atomic Pattern Combination**
   - Test: Combine two atomic patterns
   - Expected: Result has combined value and empty elements
   - Test: Combine atomic pattern with pattern having elements
   - Expected: Result has combined value and elements from non-atomic pattern

2. **Pattern with Elements Combination**
   - Test: Combine two patterns with elements
   - Expected: Result has combined value and concatenated elements in order
   - Test: Combine patterns with different element counts
   - Expected: All elements concatenated in order

3. **Nested Pattern Combination**
   - Test: Combine patterns with nested structures
   - Expected: Nested structures preserved as elements in combined pattern
   - Test: Combine patterns with different nesting depths
   - Expected: Structures preserved, elements concatenated

4. **Value Type Semigroup Integration**
   - Test: Combine patterns with String values (concatenation)
   - Expected: Values concatenate, elements concatenate
   - Test: Combine patterns with Sum Int values (addition)
   - Expected: Values add, elements concatenate
   - Test: Combine patterns with Product Int values (multiplication)
   - Expected: Values multiply, elements concatenate

### Property-Based Test Requirements

1. **Semigroup Associativity Law**
   - Property: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)` for all patterns `p1`, `p2`, `p3`
   - Test: Generate random patterns and verify associativity
   - Test: Test with different value types (String, Int, custom Semigroup instances)

2. **Element Order Preservation**
   - Property: `elements (p1 <> p2) = elements p1 ++ elements p2`
   - Test: Verify element order is preserved for all pattern structures
   - Test: Test with patterns of different sizes and nesting depths

3. **Value Combination**
   - Property: `value (p1 <> p2) = value p1 <> value p2`
   - Test: Verify value combination uses value type's Semigroup instance
   - Test: Test with different value types and Semigroup instances

4. **Structure Preservation**
   - Property: Combined pattern preserves decorated sequence model
   - Test: Verify elements form the pattern, value is decoration
   - Test: Verify nested structures are preserved correctly

### Integration Test Requirements

1. **Standard Semigroup Combinators**
   - Test: Use `sconcat` with list of patterns
   - Expected: All patterns combined correctly
   - Test: Use `stimes` to repeat a pattern
   - Expected: Pattern repeated correctly with combined values and repeated elements

2. **Edge Cases**
   - Test: Combine atomic patterns
   - Expected: Correct combination with empty elements
   - Test: Combine patterns with deeply nested structures
   - Expected: Nested structures preserved
   - Test: Combine patterns with different nesting depths
   - Expected: Structures preserved, elements concatenated

3. **Type Constraint Verification**
   - Test: Attempt to use `<>` with value type without Semigroup instance
   - Expected: Compile-time error
   - Test: Use `<>` with value type with Semigroup instance
   - Expected: Compiles and executes correctly

## Test Coverage Requirements

- **Unit tests**: Cover all edge cases (atomic patterns, different element counts, nested structures, different value types)
- **Property-based tests**: Verify Semigroup associativity law, element order preservation, value combination, structure preservation
- **Integration tests**: Verify standard Semigroup combinators (`sconcat`, `stimes`), edge cases, type constraints
- **Coverage target**: 100% of `Semigroup` instance methods covered by tests

## Performance Requirements

- **Combination complexity**: O(n+m) where n and m are the number of elements in the two patterns being combined
- **Element concatenation**: O(n+m) for list concatenation
- **Value combination**: O(1) for most value types (delegated to value type's Semigroup)
- **No performance optimizations needed**: Standard list concatenation and value combination are sufficient

