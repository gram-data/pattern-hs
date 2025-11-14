# Data Model: Semigroup Instance for Pattern

**Feature**: 010-semigroup-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

This document describes the `Semigroup` instance for the `Pattern` type, which enables combining patterns by concatenating their elements and combining their values using the value type's Semigroup instance.

## Core Entity

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
- Recursive structure enables arbitrary nesting

### Semigroup Instance

**Type Signature**:
```haskell
instance Semigroup v => Semigroup (Pattern v) where
  (<>) :: Pattern v -> Pattern v -> Pattern v
```

**Combination Semantics**:
- **Value combination**: Combine values using value type's Semigroup instance: `value (p1 <> p2) = value p1 <> value p2`
- **Element concatenation**: Concatenate elements in order: `elements (p1 <> p2) = elements p1 ++ elements p2`
- **Structure-preserving**: Preserves the decorated sequence model (elements form pattern, value is decoration)
- **Associative**: Satisfies Semigroup associativity law: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)`

**Implementation**:
```haskell
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)
```

## Combination Rules

### Rule 1: Value Combination

Values are combined using the value type's Semigroup instance:

```haskell
-- For String values (concatenation)
Pattern { value = "a", elements = [...] } <> Pattern { value = "b", elements = [...] }
-- Result: Pattern { value = "ab", elements = [...] }

-- For Sum Int values (addition)
Pattern { value = Sum 5, elements = [...] } <> Pattern { value = Sum 3, elements = [...] }
-- Result: Pattern { value = Sum 8, elements = [...] }
```

### Rule 2: Element Concatenation

Elements are concatenated in order, preserving sequence semantics:

```haskell
Pattern { value = "root", elements = [p1, p2] } <> Pattern { value = "root", elements = [p3, p4] }
-- Result: Pattern { value = "rootroot", elements = [p1, p2, p3, p4] }
```

The order is preserved: first all elements from the left pattern, then all elements from the right pattern.

### Rule 3: Nested Structure Preservation

When combining patterns with nested structures, the nested structure is preserved:

```haskell
Pattern { value = "a", elements = [Pattern { value = "b", elements = [p1] }] }
<> Pattern { value = "c", elements = [Pattern { value = "d", elements = [p2] }] }
-- Result: Pattern { value = "ac", elements = [Pattern { value = "b", elements = [p1] }, Pattern { value = "d", elements = [p2] }] }
```

Nested patterns are preserved as elements in the combined pattern.

### Rule 4: Associativity

The combination operation is associative:

```haskell
(p1 <> p2) <> p3 = p1 <> (p2 <> p3)
```

This enables safe use of `sconcat` and other Semigroup combinators.

## Edge Cases

### Atomic Patterns (No Elements)

Combining atomic patterns produces a pattern with combined value and concatenated (empty) elements:

```haskell
Pattern { value = "a", elements = [] } <> Pattern { value = "b", elements = [] }
-- Result: Pattern { value = "ab", elements = [] }
```

### Atomic Pattern with Pattern Having Elements

Combining an atomic pattern with a pattern that has elements:

```haskell
Pattern { value = "a", elements = [] } <> Pattern { value = "b", elements = [p1, p2] }
-- Result: Pattern { value = "ab", elements = [p1, p2] }
```

The atomic pattern contributes its value but no elements.

### Different Element Counts

Combining patterns with different element counts:

```haskell
Pattern { value = "a", elements = [p1] } <> Pattern { value = "b", elements = [p2, p3, p4] }
-- Result: Pattern { value = "ab", elements = [p1, p2, p3, p4] }
```

Elements are concatenated regardless of count.

### Different Nesting Depths

Combining patterns with different nesting depths:

```haskell
Pattern { value = "a", elements = [p1] } 
<> Pattern { value = "b", elements = [Pattern { value = "c", elements = [p2, p3] }] }
-- Result: Pattern { value = "ab", elements = [p1, Pattern { value = "c", elements = [p2, p3] }] }
```

Nested structures are preserved as elements in the combined pattern.

### Type Constraint

The `Semigroup` instance requires `Semigroup v` constraint:

```haskell
-- This compiles (String has Semigroup instance)
pattern "a" <> pattern "b" :: Pattern String

-- This doesn't compile if CustomType doesn't have Semigroup instance
pattern customValue1 <> pattern customValue2 :: Pattern CustomType
-- Error: No instance for (Semigroup CustomType)
```

## Validation Rules

### Semigroup Associativity Law

The `Semigroup` instance must satisfy:

```haskell
(p1 <> p2) <> p3 = p1 <> (p2 <> p3)
```

This must hold for all patterns `p1`, `p2`, `p3` of type `Pattern v` where `Semigroup v`.

### Element Order Preservation

The combination operation must preserve element order:

```haskell
elements (p1 <> p2) = elements p1 ++ elements p2
```

This ensures sequence semantics are maintained.

### Decorated Sequence Model Alignment

The combination operation must align with the decorated sequence model:

- **Elements form the pattern**: Concatenation extends the pattern sequence
- **Value is decoration**: Value combination provides decoration about the combined pattern

## Relationships

### Relationship to Value Type's Semigroup

The `Semigroup` instance for `Pattern` delegates value combination to the value type's Semigroup instance:

```haskell
value (p1 <> p2) = value p1 <> value p2
```

This respects the value type's own combination semantics.

### Relationship to List Concatenation

Element concatenation uses standard list concatenation:

```haskell
elements (p1 <> p2) = elements p1 ++ elements p2
```

This preserves sequence order and maintains the pattern structure.

### Relationship to Standard Semigroup Combinators

The `Semigroup` instance enables standard Semigroup combinators:

- `sconcat :: NonEmpty (Pattern v) -> Pattern v`: Combines a non-empty list of patterns
- `stimes :: Integral n => n -> Pattern v -> Pattern v`: Repeats a pattern n times

### Relationship to Existing Operations

**Functor (`fmap`)**: Transforms values, preserves structure. Semigroup combines patterns, creating new structure. ✅ Distinct operations.

**Foldable (`foldr`, `foldl`)**: Aggregates values from pattern. Semigroup combines patterns themselves. ✅ Distinct operations.

**Traversable (`traverse`)**: Applies effects to values. Semigroup combines patterns without effects. ✅ Distinct operations.

## State Transitions

N/A - `Semigroup` instance is a pure function with no state.

## Notes

- The `Semigroup` instance is straightforward: combine values, concatenate elements
- No additional state or caching is needed
- The implementation follows standard Haskell conventions for recursive types
- Performance is O(n+m) where n and m are the number of elements in the two patterns being combined
- The instance enables incremental pattern construction using standard Haskell combinators

