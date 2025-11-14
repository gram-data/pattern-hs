# Tasks: Predicate-Based Pattern Matching

**Input**: Design documents from `/specs/012-predicate-matching/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Tests are REQUIRED. All predicate and matching functions must have comprehensive test coverage including unit tests, property-based tests, and integration tests.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `tests/` at repository root
- Predicate and matching functions in `src/Pattern/Core.hs`
- Tests in `tests/Spec/Pattern/CoreSpec.hs` and `tests/Spec/Pattern/Properties.hs`

---

## Phase 1: User Story 1 - Query Patterns by Value Properties (Priority: P1) 🎯 MVP

**Goal**: Implement value predicate functions (`anyValue`, `allValues`) that check if values in patterns satisfy predicates. This enables basic value-based querying of patterns.

**Independent Test**: Can be fully tested by checking if any or all values in patterns satisfy predicates, testing on atomic patterns, nested patterns, and edge cases. This delivers immediate value for validation, filtering, and conditional logic based on value properties.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total. See Testing Performance Guidelines section below.

- [x] T001 [P] [US1] Write unit test for `anyValue` with atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T002 [P] [US1] Write unit test for `anyValue` with nested pattern containing matching value in tests/Spec/Pattern/CoreSpec.hs
- [x] T003 [P] [US1] Write unit test for `anyValue` with pattern containing no matching values in tests/Spec/Pattern/CoreSpec.hs
- [x] T004 [P] [US1] Write unit test for `allValues` with atomic pattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T005 [P] [US1] Write unit test for `allValues` with nested pattern where all values match in tests/Spec/Pattern/CoreSpec.hs
- [x] T006 [P] [US1] Write unit test for `allValues` with pattern where some values don't match in tests/Spec/Pattern/CoreSpec.hs
- [x] T007 [P] [US1] Write unit test for `allValues` with empty pattern (vacuous truth) in tests/Spec/Pattern/CoreSpec.hs
- [x] T008 [P] [US1] Write unit test for `anyValue` and `allValues` with deeply nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T009 [P] [US1] Write property-based test for `anyValue p = not (allValues (not . p))` in tests/Spec/Pattern/Properties.hs
- [x] T010 [P] [US1] Write property-based test for `anyValue (const True) = True` in tests/Spec/Pattern/Properties.hs
- [x] T011 [P] [US1] Write property-based test for `allValues (const False) = False` for non-empty patterns in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 1

- [x] T012 [US1] Implement `anyValue :: (v -> Bool) -> Pattern v -> Bool` in src/Pattern/Core.hs using `Foldable.toList`
- [x] T013 [US1] Implement `allValues :: (v -> Bool) -> Pattern v -> Bool` in src/Pattern/Core.hs using `Foldable.toList`
- [x] T014 [US1] Add Haddock documentation for `anyValue` with examples in src/Pattern/Core.hs
- [x] T015 [US1] Add Haddock documentation for `allValues` with examples in src/Pattern/Core.hs
- [x] T016 [US1] Export `anyValue` and `allValues` from Pattern.Core module in src/Pattern/Core.hs
- [x] T017 [US1] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 1 tests pass
- [x] T018 [US1] Git commit: "feat: implement value predicate functions (anyValue, allValues) - US1"

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Value predicate functions enable querying patterns by value properties.

---

## Phase 2: User Story 2 - Find and Filter Patterns by Structure (Priority: P2)

**Goal**: Implement pattern predicate functions (`filterPatterns`, `findPattern`, `findAllPatterns`) that find and filter subpatterns matching pattern-level criteria. This enables pattern analysis, extraction, and transformation workflows.

**Independent Test**: Can be fully tested by searching through all subpatterns (including root) and returning those that match a predicate, testing on various structures and edge cases. This delivers the ability to find and filter patterns based on pattern-level criteria.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [ ] T019 [P] [US2] Write unit test for `filterPatterns` with predicate matching some subpatterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T020 [P] [US2] Write unit test for `filterPatterns` with predicate matching root pattern in tests/Spec/Pattern/CoreSpec.hs
- [ ] T021 [P] [US2] Write unit test for `filterPatterns` with predicate matching no subpatterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T022 [P] [US2] Write unit test for `findPattern` with predicate matching first subpattern in tests/Spec/Pattern/CoreSpec.hs
- [ ] T023 [P] [US2] Write unit test for `findPattern` with predicate matching root pattern in tests/Spec/Pattern/CoreSpec.hs
- [ ] T024 [P] [US2] Write unit test for `findPattern` with predicate matching no subpatterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T025 [P] [US2] Write unit test for `findAllPatterns` with predicate matching multiple subpatterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T026 [P] [US2] Write unit test for pattern predicates on deeply nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T027 [P] [US2] Write unit test for pattern predicates on atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [ ] T028 [P] [US2] Write unit test for pattern predicates matching element sequence structure (e.g., a, b, b, a) in tests/Spec/Pattern/CoreSpec.hs
- [ ] T029 [P] [US2] Write property-based test for `filterPatterns (const True)` returns all subpatterns in tests/Spec/Pattern/Properties.hs
- [ ] T030 [P] [US2] Write property-based test for `filterPatterns (const False)` returns empty list in tests/Spec/Pattern/Properties.hs
- [ ] T031 [P] [US2] Write property-based test for `findPattern p` returns `Just` first match from `filterPatterns p` in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 2

- [x] T032 [US2] Implement `filterPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]` with recursive traversal in src/Pattern/Core.hs
- [x] T033 [US2] Implement `findPattern :: (Pattern v -> Bool) -> Pattern v -> Maybe (Pattern v)` with recursive traversal in src/Pattern/Core.hs
- [x] T034 [US2] Implement `findAllPatterns :: (Pattern v -> Bool) -> Pattern v -> [Pattern v]` (same as `filterPatterns`) in src/Pattern/Core.hs
- [x] T035 [US2] Add Haddock documentation for `filterPatterns` with examples in src/Pattern/Core.hs
- [x] T036 [US2] Add Haddock documentation for `findPattern` with examples in src/Pattern/Core.hs
- [x] T037 [US2] Add Haddock documentation for `findAllPatterns` with examples in src/Pattern/Core.hs
- [x] T038 [US2] Export `filterPatterns`, `findPattern`, and `findAllPatterns` from Pattern.Core module in src/Pattern/Core.hs
- [x] T039 [US2] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 2 tests pass
- [x] T040 [US2] Git commit: "feat: implement pattern predicate functions (filterPatterns, findPattern, findAllPatterns) - US2"

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Pattern predicate functions enable finding and filtering subpatterns based on structural criteria.

---

## Phase 3: User Story 3 - Match Patterns by Structure (Priority: P3)

**Goal**: Implement structural matching functions (`matches`, `contains`) that perform structural pattern matching beyond exact equality. This enables pattern analysis and comparison operations.

**Independent Test**: Can be fully tested by checking structural matching and subpattern containment, testing on various pattern structures and edge cases. This delivers the ability to perform structural pattern matching operations.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**
> 
> **PERFORMANCE**: Always use timeouts when running tests (`timeout 60 cabal test` or equivalent). Tests should complete in <1 minute total.

- [x] T041 [P] [US3] Write unit test for `matches` with identical patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T042 [P] [US3] Write unit test for `matches` with patterns having different values in tests/Spec/Pattern/CoreSpec.hs
- [x] T043 [P] [US3] Write unit test for `matches` with patterns having different element counts in tests/Spec/Pattern/CoreSpec.hs
- [x] T044 [P] [US3] Write unit test for `matches` with patterns having same flattened values but different structures in tests/Spec/Pattern/CoreSpec.hs
- [x] T045 [P] [US3] Write unit test for `matches` with atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T046 [P] [US3] Write unit test for `contains` with pattern containing subpattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T047 [P] [US3] Write unit test for `contains` with pattern not containing subpattern in tests/Spec/Pattern/CoreSpec.hs
- [x] T048 [P] [US3] Write unit test for `contains` with pattern containing itself (self-containment) in tests/Spec/Pattern/CoreSpec.hs
- [x] T049 [P] [US3] Write unit test for `contains` with atomic patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T050 [P] [US3] Write unit test for structural matching on deeply nested patterns in tests/Spec/Pattern/CoreSpec.hs
- [x] T051 [P] [US3] Write property-based test for `matches` reflexivity: `matches p p = True` in tests/Spec/Pattern/Properties.hs
- [x] T052 [P] [US3] Write property-based test for `matches` symmetry: `matches p1 p2 = matches p2 p1` in tests/Spec/Pattern/Properties.hs
- [x] T053 [P] [US3] Write property-based test for `contains` reflexivity: `contains p p = True` in tests/Spec/Pattern/Properties.hs
- [x] T054 [P] [US3] Write property-based test for `contains` transitivity in tests/Spec/Pattern/Properties.hs

### Implementation for User Story 3

- [x] T055 [US3] Implement `matches :: (Eq v) => Pattern v -> Pattern v -> Bool` with recursive structural comparison in src/Pattern/Core.hs
- [x] T056 [US3] Implement `contains :: (Eq v) => Pattern v -> Pattern v -> Bool` with recursive search in src/Pattern/Core.hs
- [x] T057 [US3] Add Haddock documentation for `matches` with examples in src/Pattern/Core.hs
- [x] T058 [US3] Add Haddock documentation for `contains` with examples in src/Pattern/Core.hs
- [x] T059 [US3] Export `matches` and `contains` from Pattern.Core module in src/Pattern/Core.hs
- [x] T060 [US3] Run tests with timeout: `timeout 60 cabal test` to verify all User Story 3 tests pass
- [x] T061 [US3] Git commit: "feat: implement structural matching functions (matches, contains) - US3"

**Checkpoint**: All user stories should now be independently functional. Structural matching functions enable pattern comparison and containment checking.

---

## Phase 4: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final integration

- [x] T062 [P] Update module-level documentation in src/Pattern/Core.hs to include predicate and matching functions
- [x] T063 [P] Add integration tests verifying predicate functions work with existing Pattern operations in tests/Spec/Pattern/CoreSpec.hs
- [x] T064 [P] Add integration tests verifying predicate functions work with typeclass instances (Functor, Foldable) in tests/Spec/Pattern/CoreSpec.hs
- [x] T065 [P] Add edge case tests for all functions (100+ nesting levels, 1000+ nodes) in tests/Spec/Pattern/CoreSpec.hs
- [x] T066 [P] Verify performance targets (1000 nodes, 100 levels depth) for all functions
- [x] T067 Run full test suite with timeout: `timeout 60 cabal test` to verify all tests pass
- [x] T068 Run quickstart.md validation to ensure examples work correctly
- [x] T069 Git commit: "docs: finalize predicate-based pattern matching feature"

---

## Dependencies & Execution Order

### Phase Dependencies

- **User Story 1 (Phase 1)**: No dependencies - can start immediately
- **User Story 2 (Phase 2)**: Can start after User Story 1 - independently testable
- **User Story 3 (Phase 3)**: Can start after User Story 1 - independently testable (may use `matches` for `contains` implementation)
- **Polish (Phase 4)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: No dependencies - MVP functionality
- **User Story 2 (P2)**: No dependencies on other stories - independently testable
- **User Story 3 (P3)**: No dependencies on other stories - independently testable (though `contains` uses `matches` internally)

### Within Each User Story

- Tests MUST be written and FAIL before implementation (TDD)
- Implementation tasks follow test tasks
- Documentation added after implementation
- Export statements added after implementation
- Tests run after implementation to verify
- Git commit after story completion

### Parallel Opportunities

- All test tasks marked [P] within a user story can run in parallel
- Test tasks and implementation tasks cannot run in parallel (tests must fail first)
- Different user stories can be worked on in parallel by different team members (after US1 completes)
- Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write unit test for anyValue with atomic pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for anyValue with nested pattern containing matching value in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for anyValue with pattern containing no matching values in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for allValues with atomic pattern in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for allValues with nested pattern where all values match in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for allValues with pattern where some values don't match in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for allValues with empty pattern (vacuous truth) in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write unit test for anyValue and allValues with deeply nested patterns in tests/Spec/Pattern/CoreSpec.hs"
Task: "Write property-based test for anyValue p = not (allValues (not . p)) in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for anyValue (const True) = True in tests/Spec/Pattern/Properties.hs"
Task: "Write property-based test for allValues (const False) = False for non-empty patterns in tests/Spec/Pattern/Properties.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: User Story 1 (value predicate functions)
2. **STOP and VALIDATE**: Test User Story 1 independently
3. Git commit after User Story 1
4. Deploy/demo if ready

### Incremental Delivery

1. Add User Story 1 → Test independently → Git commit → Deploy/Demo (MVP!)
2. Add User Story 2 → Test independently → Git commit → Deploy/Demo
3. Add User Story 3 → Test independently → Git commit → Deploy/Demo
4. Polish phase → Git commit → Final release
5. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes User Story 1 together (foundational)
2. Once User Story 1 is done:
   - Developer A: User Story 2
   - Developer B: User Story 3
3. Stories complete and integrate independently
4. Each developer commits their story independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (TDD approach)
- **Git commit after each user story completion** (tasks T018, T040, T061)
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- **CRITICAL**: Always use timeouts when running tests to prevent hanging
- **CRITICAL**: Property-based tests MUST use `quickProperty` helper (20 test cases max)
- **CRITICAL**: Property-based tests MUST complete in <10ms total

## Testing Performance Guidelines

### Test Execution Timeouts

**CRITICAL**: Always use timeouts when running tests to prevent hanging:

- **First test run after implementation**: Use `timeout 60 cabal test` (60 seconds) to catch any infinite loops or performance issues
- **Subsequent test runs**: Use `timeout 30 cabal test` (30 seconds) for normal verification
- **Individual test runs**: Use `timeout 30 cabal test --test-options="--match 'Test Name'"`

### Test Performance Requirements

- **Unit tests**: Each test should complete in <100ms
- **Property-based tests**: Each property test MUST use `quickProperty` helper (20 test cases max) and complete in <10ms total
- **Full test suite**: Should complete in <1 minute total
- **Individual test phases**: Should complete in <10 seconds

### Troubleshooting Slow Tests

If tests hang or take too long:

1. **Check for infinite recursion**: Verify recursive functions have proper base cases
2. **Check pattern generator size**: Ensure property-based tests use bounded generators (max depth: 3, max elements: 5)
3. **Reduce test cases**: Reduce `quickProperty` test cases from 20 to 10 if needed
4. **Check for ambiguous function calls**: Use explicit module qualifiers
5. **Verify test isolation**: Ensure tests don't depend on shared mutable state

### Test Execution Commands

```bash
# First run after implementation (with 60 second timeout):
timeout 60 cabal test

# Normal verification (with 30 second timeout):
timeout 30 cabal test

# Individual test runs (with 30 second timeout):
timeout 30 cabal test --test-options="--match 'Test Name'"
```

### Performance Monitoring

- Monitor test execution time in CI/CD
- Alert if test suite exceeds 1 minute
- Investigate any test that takes >1 second individually
- Use profiling tools if tests consistently slow

