# Feature Specification: Predicate-Based Pattern Matching

**Feature Branch**: `012-predicate-matching`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Work on Feature 9: Predicate-Based Pattern Matching of @TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Query Patterns by Value Properties (Priority: P1)

As a developer using the Pattern library, I need to check whether patterns contain values that satisfy specific conditions, so that I can filter and query patterns based on value properties rather than exact equality.

**Why this priority**: Value-based predicates are the most fundamental query capability. They enable developers to ask questions like "does this pattern contain any negative numbers?" or "are all values in this pattern valid?" without needing to know the exact structure or values. This is essential for validation, filtering, and conditional logic based on value properties.

**Independent Test**: Can be fully tested by implementing functions that check if any or all values in a pattern satisfy a predicate function, testing on atomic patterns, nested patterns, and edge cases. Delivers the ability to query patterns based on value properties.

**Acceptance Scenarios**:

1. **Given** a pattern with values, **When** checking if any value satisfies a predicate (e.g., `> 0`), **Then** the function returns `True` if at least one value matches, `False` otherwise
2. **Given** a pattern with values, **When** checking if all values satisfy a predicate (e.g., `> 0`), **Then** the function returns `True` only if every value matches, `False` otherwise
3. **Given** an atomic pattern (single value, no elements), **When** applying value predicates, **Then** the predicate is evaluated on the pattern's value
4. **Given** a nested pattern with multiple levels, **When** applying value predicates, **Then** all values at all nesting levels are considered
5. **Given** a pattern where no values match the predicate, **When** checking if any value matches, **Then** it returns `False`
6. **Given** a pattern where all values match the predicate, **When** checking if all values match, **Then** it returns `True`

---

### User Story 2 - Find and Filter Patterns by Structure (Priority: P2)

As a developer using the Pattern library, I need to find specific subpatterns within a pattern structure that match certain criteria, so that I can extract relevant pattern components based on structural or value-based conditions.

**Why this priority**: Pattern-based predicates enable finding and filtering subpatterns based on their structure, values, or both. This is essential for pattern analysis, extraction, and transformation workflows where developers need to locate specific pattern components within larger structures.

**Independent Test**: Can be fully tested by implementing functions that search through all subpatterns (including the root) and return those that match a predicate, testing on various structures and edge cases. Delivers the ability to find and filter patterns based on pattern-level criteria.

**Acceptance Scenarios**:

1. **Given** a pattern structure, **When** filtering patterns that match a predicate, **Then** all matching subpatterns (including root and nested) are returned as a list
2. **Given** a pattern structure, **When** finding the first pattern that matches a predicate, **Then** the first matching subpattern is returned, or no result if none match
3. **Given** a pattern structure, **When** finding all patterns that match a predicate, **Then** all matching subpatterns are returned, including duplicates if a pattern appears multiple times
4. **Given** a pattern where the root pattern matches the predicate, **When** filtering or finding, **Then** the root pattern is included in results
5. **Given** a deeply nested pattern structure, **When** filtering or finding, **Then** all nested subpatterns at all depths are considered
6. **Given** a pattern where no subpatterns match the predicate, **When** filtering or finding, **Then** empty list or no result is returned appropriately

---

### User Story 3 - Match Patterns by Structure (Priority: P3)

As a developer using the Pattern library, I need to check if one pattern matches or contains another pattern structurally, so that I can perform structural pattern matching beyond exact equality.

**Why this priority**: Structural pattern matching enables checking if patterns have similar structures or if one pattern contains another as a subpattern. This extends beyond `Eq` (which requires exact equality) to support partial matching and containment queries, which are essential for pattern analysis and comparison.

**Independent Test**: Can be fully tested by implementing functions that check structural matching and subpattern containment, testing on various pattern structures and edge cases. Delivers the ability to perform structural pattern matching operations.

**Acceptance Scenarios**:

1. **Given** two patterns, **When** checking if one matches the other structurally, **Then** the function returns `True` if they have matching structure (value and elements recursively), `False` otherwise
2. **Given** two patterns, **When** checking if one contains the other as a subpattern, **Then** the function returns `True` if the second pattern appears anywhere in the first pattern's structure, `False` otherwise
3. **Given** a pattern and itself, **When** checking structural matching, **Then** it returns `True` (self-matching)
4. **Given** a pattern and itself, **When** checking containment, **Then** it returns `True` (pattern contains itself)
5. **Given** a pattern and an empty/atomic pattern, **When** checking containment, **Then** the result correctly identifies if the atomic pattern appears in the structure
6. **Given** patterns with different structures but same flattened values, **When** checking structural matching, **Then** the function distinguishes based on structure, not just values

---

### Edge Cases

- What happens when applying value predicates to an atomic pattern (single value, no elements)?
- What happens when applying value predicates to a pattern where all values match the predicate?
- What happens when applying value predicates to a pattern where no values match the predicate?
- What happens when filtering patterns with a predicate that matches the root pattern?
- What happens when filtering patterns with a predicate that matches no subpatterns?
- What happens when filtering patterns with a predicate that matches all subpatterns?
- What happens when finding patterns in deeply nested structures (100+ levels)?
- What happens when checking structural matching between patterns with identical values but different structures?
- What happens when checking containment where the subpattern appears multiple times in the structure?
- What happens when checking structural matching or containment with empty/atomic patterns?
- What happens when pattern predicates consider both structure and values versus only structure or only values?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a function that checks if any value in a pattern satisfies a predicate function
- **FR-002**: System MUST provide a function that checks if all values in a pattern satisfy a predicate function
- **FR-003**: Value predicate functions MUST consider all values at all nesting levels in the pattern structure
- **FR-004**: Value predicate functions MUST handle atomic patterns (evaluating predicate on the pattern's value)
- **FR-005**: System MUST provide a function that filters all subpatterns (including root) that match a pattern predicate
- **FR-006**: System MUST provide a function that finds the first subpattern (including root) that matches a pattern predicate, returning no result if none match
- **FR-007**: System MUST provide a function that finds all subpatterns (including root) that match a pattern predicate
- **FR-008**: Pattern predicate functions MUST consider the root pattern and all nested subpatterns at all depths
- **FR-009**: System MUST provide a function that checks if one pattern matches another structurally (value and elements recursively)
- **FR-010**: System MUST provide a function that checks if one pattern contains another as a subpattern anywhere in its structure
- **FR-011**: Structural matching functions MUST distinguish patterns based on structure, not just flattened values
- **FR-012**: Structural matching functions MUST handle self-matching (pattern matches itself) and self-containment (pattern contains itself)
- **FR-013**: All predicate and matching functions MUST handle edge cases: atomic patterns, empty elements, deeply nested structures, no matches, all matches
- **FR-014**: System MUST provide comprehensive tests verifying predicate matching on atomic patterns, nested patterns, and edge cases
- **FR-015**: System MUST document predicate and matching function semantics with clear examples showing usage patterns and expected behavior

### Key Entities *(include if feature involves data)*

- **Pattern**: A recursive structure representing a decorated sequence, where elements form the pattern and value provides decoration. Patterns can be atomic (no elements) or contain nested patterns as elements.
- **Value Predicate**: A function that takes a value and returns a boolean, used to test individual values within patterns.
- **Pattern Predicate**: A function that takes a pattern and returns a boolean, used to test entire pattern structures (including value and elements).
- **Subpattern**: Any pattern that appears within another pattern's structure, including the root pattern itself and all nested patterns at any depth.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can query patterns by value properties, with value predicate functions correctly identifying matches in 100% of test cases across atomic, nested, and edge case patterns
- **SC-002**: Developers can find and filter subpatterns by structure, with pattern predicate functions correctly identifying all matching subpatterns in 100% of test cases, including root and nested patterns
- **SC-003**: Developers can perform structural pattern matching, with matching functions correctly distinguishing patterns based on structure (not just values) in 100% of test cases
- **SC-004**: All predicate and matching functions handle edge cases correctly (atomic patterns, empty elements, deeply nested structures, no matches, all matches) with 100% test coverage
- **SC-005**: Predicate and matching operations complete in reasonable time for patterns with up to 1000 nodes and nesting depth up to 100 levels
- **SC-006**: All predicate and matching functions are documented with clear examples showing usage patterns and expected behavior

## Assumptions

- Value predicates operate on individual values only, not considering structural context (consistent with Foldable semantics where values are extracted and tested independently)
- Pattern predicates can consider both structure and values, allowing flexible matching criteria (e.g., "find all patterns with depth > 2" or "find all patterns where value equals X")
- Structural matching requires exact structural correspondence (value and elements recursively), distinguishing patterns with same flattened values but different structures
- Subpattern containment checks if a pattern appears anywhere in another pattern's structure, including as the root pattern itself
- All predicate functions traverse the entire pattern structure (all nesting levels) to ensure comprehensive matching
- Pattern predicate functions (filterPatterns, findPattern, findAllPatterns) include the root pattern in their search scope, not just nested subpatterns
