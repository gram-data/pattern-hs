# Tasks: Semigroup Instance for Pattern

**Input**: Design documents from `/specs/010-semigroup-instance/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are REQUIRED. The `Semigroup` instance must have comprehensive test coverage including unit tests, property-based tests, and integration tests.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- `Semigroup` instance in `src/Pattern/Core.hs`
- Tests in `tests/Spec/Pattern/CoreSpec.hs` and `tests/Spec/Pattern/Properties.hs`

---

## Phase 1: Prerequisites (Already Complete)

**Note**: User Stories 1 and 2 (evaluation and design) have been completed during the research and design phases. The use cases have been identified, combination semantics have been designed, and alignment with the decorated sequence model has been verified. These are prerequisites for implementation.

**User Story 1 - Evaluate Use Cases**: ✅ Complete (documented in `research.md`)
- Three concrete use cases identified: incremental pattern construction, pattern merging in graph operations, pattern accumulation in transformations
- Semantic alignment with decorated sequence model verified
- Distinct value beyond existing operations confirmed

**User Story 2 - Design Semigroup Combination Semantics**: ✅ Complete (documented in `research.md` and `data-model.md`)
- Combination semantics defined: `value (p1 <> p2) = value p1 <> value p2`, `elements (p1 <> p2) = elements p1 ++ elements p2`
- Associativity law verified: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)`
- Alignment with decorated sequence model confirmed

---

## Phase 2: User Story 3 - Implement Semigroup Instance (Priority: P3) 🎯 MVP

**Goal**: Implement `Semigroup` instance for `Pattern` that combines patterns by concatenating their elements and combining their values using the value type's Semigroup instance. This enables incremental pattern construction using standard Haskell combinators.

**Independent Test**: Can be fully tested by combining patterns with `<>`, verifying that values combine correctly and elements concatenate in order, and ensuring the operation compiles with `Semigroup v` constraint. This delivers immediate value for pattern combination operations.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T001 [P] [US3] Write unit test for combining two atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US3] Write unit test for combining atomic pattern with pattern having elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US3] Write unit test for combining two patterns with elements in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [P] [US3] Write unit test for combining patterns with different element counts in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [P] [US3] Write unit test for combining patterns with String values (concatenation) in tests/Spec/Pattern/CoreSpec.hs
- [x] T006 [P] [US3] Write unit test for combining patterns with Sum Int values (addition) in tests/Spec/Pattern/CoreSpec.hs
- [x] T007 [P] [US3] Write unit test for combining patterns with Product Int values (multiplication) in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US3] Write unit test for element order preservation in tests/Spec/Pattern/CoreSpec.hs
- [x] T009 [P] [US3] Write unit test for value combination using value type's Semigroup in tests/Spec/Pattern/CoreSpec.hs
- [x] T010 [P] [US3] Write unit test for type constraint: `Semigroup v` requirement in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 3

- [x] T011 [US3] Implement `Semigroup` instance for `Pattern v` with `<>` operator in src/Pattern/Core.hs
- [x] T012 [US3] Add comprehensive Haddock documentation with examples for `Semigroup` instance in src/Pattern/Core.hs
- [x] T013 [US3] Verify `Semigroup` instance is exported from Pattern.Core module in src/Pattern/Core.hs
- [x] T014 [US3] Verify all tests pass for `Semigroup` instance

**Checkpoint**: ✅ At this point, User Story 3 should be fully functional and testable independently. `Semigroup` instance works correctly for combining patterns with value combination and element concatenation.

---

## Phase 3: User Story 4 - Verify Semigroup Laws and Edge Cases (Priority: P3)

**Goal**: Verify that the `Semigroup` instance satisfies the associativity law and handles all edge cases correctly, ensuring robust behavior for production use.

**Independent Test**: Can be fully tested by writing property-based tests for associativity law and unit tests for edge cases (atomic patterns, nested structures, different nesting depths). This delivers value for ensuring mathematical correctness and handling all pattern structures.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T015 [P] [US4] Write property-based test for associativity law: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)` in tests/Spec/Pattern/Properties.hs
- [x] T016 [P] [US4] Write property-based test for associativity with different value types (String, Sum Int, Product Int) in tests/Spec/Pattern/Properties.hs
- [x] T017 [P] [US4] Write property-based test for element order preservation: `elements (p1 <> p2) = elements p1 ++ elements p2` in tests/Spec/Pattern/Properties.hs
- [x] T018 [P] [US4] Write property-based test for value combination: `value (p1 <> p2) = value p1 <> value p2` in tests/Spec/Pattern/Properties.hs
- [x] T019 [P] [US4] Write property-based test for structure preservation in tests/Spec/Pattern/Properties.hs
- [x] T020 [P] [US4] Write unit test for combining nested patterns (preserving nested structure) in tests/Spec/Pattern/CoreSpec.hs
- [x] T021 [P] [US4] Write unit test for combining patterns with different nesting depths in tests/Spec/Pattern/CoreSpec.hs
- [x] T022 [P] [US4] Write unit test for combining patterns with deeply nested structures (10+ levels) in tests/Spec/Pattern/CoreSpec.hs
- [x] T023 [P] [US4] Write unit test for combining patterns with many elements (100+ elements) in tests/Spec/Pattern/CoreSpec.hs

### Implementation for User Story 4

- [x] T024 [US4] Verify `Semigroup` instance satisfies associativity law (no additional implementation needed, verify existing instance)
- [x] T025 [US4] Verify all tests pass for associativity law and edge cases

**Checkpoint**: ✅ At this point, User Stories 3 AND 4 should both work independently. `Semigroup` instance satisfies associativity law and handles all edge cases correctly.

---

## Phase 4: Integration & Standard Combinators

**Purpose**: Verify integration with standard Semigroup combinators and existing Pattern operations

### Integration Tests

- [x] T026 [P] Write integration test for `sconcat` with list of patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T027 [P] Write integration test for `stimes` to repeat a pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T028 [P] Write integration test for `Semigroup` instance with pattern constructors (`pattern`, `patternWith`, `fromList`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T029 [P] Write integration test for `Semigroup` instance with type class instances (`Functor`, `Foldable`, `Traversable`) in tests/Spec/Pattern/CoreSpec.hs
- [x] T030 [P] Write integration test for combining patterns with non-commutative value type Semigroup in tests/Spec/Pattern/CoreSpec.hs

### Validation & Documentation

- [x] T031 Review and update module-level documentation in src/Pattern/Core.hs to include `Semigroup` instance
- [x] T032 Verify `Semigroup` instance is re-exported from main Pattern module in src/Pattern.hs
- [x] T033 Verify all tests pass (run full test suite with timeout)

**Checkpoint**: ✅ At this point, all integration tests pass, documentation is updated, and `Semigroup` instance is fully integrated with the Pattern library.

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 3 (Phase 2)**: Depends on User Stories 1 and 2 completion (prerequisites already complete from research/design phase)
- **User Story 4 (Phase 3)**: Depends on User Story 3 completion - requires `Semigroup` instance to be implemented
- **Integration (Phase 4)**: Depends on all user stories completion - comprehensive validation

### User Story Dependencies

```
User Story 1 (Evaluation) ✅ Complete
    ↓
User Story 2 (Design) ✅ Complete
    ↓
User Story 3 (Implementation) → User Story 4 (Verification)
    ↓
Integration & Polish
```

### Parallel Execution Opportunities

**Within User Story 3 (Phase 2)**:
- All test tasks (T001-T010) can run in parallel - they test different aspects independently
- Implementation task (T011) must wait for tests to be written (TDD approach)

**Within User Story 4 (Phase 3)**:
- All test tasks (T015-T023) can run in parallel - they test different properties independently
- Verification task (T024) must wait for tests to be written

**Within Integration (Phase 4)**:
- All integration test tasks (T026-T030) can run in parallel - they test different integrations independently

## Implementation Strategy

### MVP Scope

**Minimum Viable Product**: User Story 3 (Implement Semigroup Instance)
- Implements core `Semigroup` instance functionality
- Provides basic test coverage
- Enables pattern combination operations
- Can be used independently for incremental pattern construction

### Incremental Delivery

1. **Phase 2 (User Story 3)**: Core implementation with basic tests
   - Delivers working `Semigroup` instance
   - Enables pattern combination operations
   - Provides foundation for further testing

2. **Phase 3 (User Story 4)**: Comprehensive verification
   - Verifies associativity law with property-based tests
   - Handles all edge cases
   - Ensures mathematical correctness

3. **Phase 4 (Integration)**: Full integration
   - Verifies standard Semigroup combinators
   - Integrates with existing Pattern operations
   - Completes documentation

## Testing Performance Guidelines

**Performance Targets**:
- Unit tests: Complete in <10 seconds total
- Property-based tests: Complete in <30 seconds total (with reasonable QuickCheck size limits)
- Integration tests: Complete in <20 seconds total
- Full test suite: Complete in <60 seconds total

**QuickCheck Configuration**:
- Use reasonable size limits (e.g., `maxSize = 10` for pattern depth, `maxSize = 20` for element count)
- Use `quickCheckWith` with timeout for property-based tests
- Focus on common cases rather than exhaustive edge cases in property tests

**Test Organization**:
- Group related tests together
- Use descriptive test names
- Include examples in test documentation
- Ensure tests are independent and can run in any order

## Success Criteria

### User Story 3 Success Criteria

- ✅ `Semigroup` instance compiles successfully with `Semigroup v` constraint
- ✅ All unit tests pass (T001-T010)
- ✅ Pattern combination works correctly: values combine, elements concatenate
- ✅ Element order is preserved
- ✅ Type constraint is enforced

### User Story 4 Success Criteria

- ✅ All property-based tests pass (T015-T019)
- ✅ Associativity law holds: `(p1 <> p2) <> p3 = p1 <> (p2 <> p3)`
- ✅ All edge case tests pass (T020-T023)
- ✅ Nested structures are preserved correctly
- ✅ Different nesting depths are handled correctly

### Integration Success Criteria

- ✅ Standard Semigroup combinators work (`sconcat`, `stimes`)
- ✅ Integration with pattern constructors works
- ✅ Integration with type class instances works
- ✅ All tests pass (full test suite)
- ✅ Documentation is complete and accurate

## Notes

- The `Semigroup` instance implementation is straightforward: combine values using value type's Semigroup, concatenate elements using list concatenation
- No additional state or caching is needed
- Performance is O(n+m) where n and m are the number of elements in the two patterns being combined
- The instance enables incremental pattern construction using standard Haskell combinators
- All tasks follow TDD approach: write tests first, then implement

