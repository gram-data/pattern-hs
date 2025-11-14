# Research: Semigroup Instance for Pattern

**Feature**: 010-semigroup-instance  
**Date**: 2025-01-27  
**Status**: Complete

## Research Objectives

1. Identify concrete use cases for combining patterns
2. Design combination semantics that align with decorated sequence model
3. Evaluate whether Semigroup provides distinct value beyond existing operations
4. Determine best practices for Semigroup instances in Haskell

## Use Case Analysis

### Use Case 1: Incremental Pattern Construction

**Scenario**: Building patterns incrementally by combining smaller patterns into larger ones.

**Example**: 
- Start with atomic patterns: `p1 = pattern "A"`, `p2 = pattern "B"`
- Combine into sequence: `p1 <> p2` produces pattern with elements `[p1, p2]`
- Continue building: `(p1 <> p2) <> pattern "C"` produces longer sequence

**Value**: Enables compositional pattern construction using standard Haskell combinators.

**Alignment with Decorated Sequence Model**: ✅ Elements concatenate to form longer sequences, values combine to provide decoration about the combined pattern.

### Use Case 2: Pattern Merging in Graph Operations

**Scenario**: When working with graph views, combining patterns that represent related graph elements.

**Example**:
- Pattern representing a node: `nodePattern = patternWith "Node" [pattern "id1"]`
- Pattern representing an edge: `edgePattern = patternWith "Edge" [pattern "id2", pattern "id3"]`
- Combined pattern: `nodePattern <> edgePattern` represents a subgraph structure

**Value**: Enables building graph structures incrementally by combining node and edge patterns.

**Alignment with Decorated Sequence Model**: ✅ Elements concatenate to form the combined graph structure, values combine to provide metadata about the combined structure.

### Use Case 3: Pattern Accumulation in Transformations

**Scenario**: Accumulating patterns during fold or traversal operations.

**Example**:
- Using `foldMap` or `foldr` to accumulate patterns from a collection
- Combining patterns using `sconcat` from a list of patterns
- Building patterns from streaming data sources

**Value**: Enables standard Haskell accumulation patterns (folds, concat) to work with Pattern types.

**Alignment with Decorated Sequence Model**: ✅ Accumulation naturally concatenates elements and combines values, preserving the decorated sequence semantic.

## Combination Semantics Design

### Decision: Value Combination

**Chosen Approach**: Combine values using value type's Semigroup instance: `value (p1 <> p2) = value p1 <> value p2`

**Rationale**: 
- Respects the value type's own combination semantics
- Enables flexible decoration combination (strings concatenate, numbers add, etc.)
- Type-safe through `Semigroup v` constraint

**Alternatives Considered**:
- Always use left value: Rejected - loses information from right pattern
- Always use right value: Rejected - loses information from left pattern
- Custom combination function: Rejected - violates Semigroup abstraction, requires additional parameter

### Decision: Element Concatenation

**Chosen Approach**: Concatenate elements in order: `elements (p1 <> p2) = elements p1 ++ elements p2`

**Rationale**:
- Preserves sequence order (essential for pattern semantics)
- Natural list concatenation aligns with decorated sequence model
- Elements form the pattern, so concatenation extends the pattern sequence

**Alternatives Considered**:
- Interleaving elements: Rejected - loses sequence order, doesn't align with pattern model
- Nested combination: Rejected - would create `Pattern { value = ..., elements = [p1, p2] }` which changes structure unnecessarily
- Set union: Rejected - patterns are sequences, not sets; order matters

### Decision: Associativity Verification

**Chosen Approach**: Verify associativity law holds: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)`

**Rationale**:
- Semigroup law requirement - must be satisfied
- Enables safe use of `sconcat` and other Semigroup combinators
- Mathematical correctness requirement

**Verification Strategy**:
- Property-based tests with QuickCheck
- Test all pattern structures: atomic, with elements, nested, different depths
- Verify for different value types (String, Int, custom Semigroup instances)

## Alignment with Decorated Sequence Model

### Elements Form the Pattern

**Verification**: ✅ Element concatenation preserves the semantic that elements form the pattern itself. When combining `p1 <> p2`, the result's elements are the concatenation of `p1.elements` and `p2.elements`, extending the pattern sequence.

### Value is Decoration

**Verification**: ✅ Value combination preserves the semantic that value is decoration about the pattern. When combining `p1 <> p2`, the result's value combines the decorations from both patterns using the value type's Semigroup instance.

### Sequence Order Preservation

**Verification**: ✅ Element concatenation maintains sequence order. `p1 <> p2` produces elements in order: first all elements from `p1`, then all elements from `p2`. This preserves the essential sequence property of patterns.

## Distinct Value Analysis

### Comparison with Existing Operations

**Functor (`fmap`)**: Transforms values, preserves structure. Semigroup combines patterns, creating new structure. ✅ Distinct value.

**Foldable (`foldr`, `foldl`)**: Aggregates values from pattern. Semigroup combines patterns themselves. ✅ Distinct value.

**Traversable (`traverse`)**: Applies effects to values. Semigroup combines patterns without effects. ✅ Distinct value.

**Construction Functions (`pattern`, `patternWith`, `fromList`)**: Create patterns from values. Semigroup combines existing patterns. ✅ Distinct value.

**Conclusion**: Semigroup provides distinct value not available through existing operations. It enables incremental pattern construction and composition, which is a fundamental operation for building complex patterns from simpler ones.

## Haskell Best Practices

### Semigroup Instance Pattern

**Standard Pattern**: For recursive types, combine by combining components recursively:

```haskell
instance Semigroup v => Semigroup (Pattern v) where
  Pattern v1 els1 <> Pattern v2 els2 = Pattern (v1 <> v2) (els1 ++ els2)
```

**Rationale**: 
- Follows standard Haskell pattern for recursive types
- Clear and straightforward implementation
- Easy to verify associativity

### Type Constraint

**Chosen**: `Semigroup v => Semigroup (Pattern v)`

**Rationale**:
- Value type must have Semigroup instance to combine values
- Standard pattern for parameterized types
- Type-safe: compiler enforces constraint

### Documentation Requirements

**Required**:
- Explain combination semantics clearly
- Provide examples showing how patterns combine
- Document associativity law satisfaction
- Explain alignment with decorated sequence model

## Implementation Strategy

### Phase 1: Evaluation and Design
- Document use cases (this research)
- Design combination semantics (this research)
- Verify alignment with decorated sequence model (this research)

### Phase 2: Implementation
- Implement Semigroup instance in `Pattern.Core`
- Add comprehensive Haddock documentation
- Write unit tests for edge cases

### Phase 3: Verification
- Write property-based tests for associativity law
- Test with different value types
- Verify standard Semigroup combinators work

## Conclusion

**Decision**: ✅ Proceed with Semigroup instance implementation

**Rationale**:
1. Three concrete use cases identified (incremental construction, graph merging, pattern accumulation)
2. Combination semantics clearly defined and align with decorated sequence model
3. Provides distinct value beyond existing operations
4. Follows standard Haskell patterns and best practices
5. Satisfies Semigroup associativity law

**Next Steps**: Proceed to Phase 1 design (data-model.md, contracts, quickstart.md)

