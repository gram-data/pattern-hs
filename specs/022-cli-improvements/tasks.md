# Tasks: CLI Tool Improvements for Language Porting

**Input**: Design documents from `/specs/022-cli-improvements/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅

**Tests**: Included - comprehensive testing as specified in plan.md (unit tests, property-based tests, integration tests)

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **CLI application**: `apps/gram-hs-cli/`
- Source: `apps/gram-hs-cli/src/GramHs/CLI/`
- Tests: `apps/gram-hs-cli/tests/Spec/CLI/`
- Manpage: `apps/gram-hs-cli/man/gram-hs.1`

---

## Phase 1: Setup (Project Verification)

**Purpose**: Verify existing project structure and configuration

- [x] T001 Verify project structure matches plan.md in apps/gram-hs-cli/
- [x] T002 Verify gram-hs-cli.cabal includes required dependencies (optparse-applicative, aeson, aeson-pretty, text, time, cryptohash-sha256, random)
- [x] T003 Verify existing CLI command structure in apps/gram-hs-cli/src/GramHs/CLI/Commands/
- [x] T004 Verify existing JSON serialization in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T005 Verify existing Output module in apps/gram-hs-cli/src/GramHs/CLI/Output.hs

---

## Phase 2: Foundational (Output Infrastructure)

**Purpose**: Core output infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### OutputOptions Type Definition

- [x] T006 [P] Define OutputOptions data type with valueOnly, deterministic, canonical fields in apps/gram-hs-cli/src/GramHs/CLI/Types.hs
- [x] T007 [P] Add defaultOutputOptions function returning all False values in apps/gram-hs-cli/src/GramHs/CLI/Types.hs
- [x] T008 [P] Add enforceDeterministicCanonical function that sets canonical=True when deterministic=True in apps/gram-hs-cli/src/GramHs/CLI/Types.hs
- [x] T009 [P] Add Haddock documentation for OutputOptions type in apps/gram-hs-cli/src/GramHs/CLI/Types.hs

### Shared Output Flag Parser

- [x] T010 [P] Create outputOptionsParser function for --value-only, --deterministic, --canonical flags in apps/gram-hs-cli/src/GramHs/CLI/Types.hs
- [x] T011 [P] Add Haddock documentation for outputOptionsParser in apps/gram-hs-cli/src/GramHs/CLI/Types.hs

### Canonical JSON Function

- [x] T012 [P] Create canonicalizeJSON function that recursively sorts all object keys in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T013 [P] Add Haddock documentation for canonicalizeJSON function in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T014 [P] Add unit test for canonicalizeJSON with nested objects in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs

**Checkpoint**: Output infrastructure complete - user story implementation can now begin

---

## Phase 3: User Story 1 - Compare Outputs Without Metadata (Priority: P1) 🎯 MVP

**Goal**: Enable `--value-only` and `--deterministic` flags to exclude or fix metadata for reliable equivalence checking between implementations.

**Independent Test**: Can be fully tested by running the CLI with `--value-only` flag on a known input, verifying that the output contains only the result value without metadata wrapper, and confirming that identical inputs produce identical outputs across multiple runs.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T015 [P] [US1] Add test for --value-only flag producing result value only (no metadata) in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T016 [P] [US1] Add test for --value-only flag with error output (error object only) in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T017 [P] [US1] Add test for --deterministic flag using fixed timestamp and hash in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T018 [P] [US1] Add test for --value-only --deterministic combination in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T019 [P] [US1] Add property test for deterministic output (same input → same output) in apps/gram-hs-cli/tests/Spec/Properties/DeterministicSpec.hs
- [x] T020 [P] [US1] Add integration test for parse command with --value-only flag in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs

### Implementation for User Story 1

- [x] T021 [US1] Modify patternToJSON to accept OutputOptions parameter in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T022 [US1] Implement value-only output logic (exclude Meta, Result, Diagnostics wrappers) in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T023 [US1] Implement deterministic metadata logic (fixed timestamp "1970-01-01T00:00:00+0000", fixed hash zeros) in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T024 [US1] Modify errorToJSON to accept OutputOptions parameter in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T025 [US1] Implement value-only error output logic in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T026 [US1] Modify formatOutput to accept and pass OutputOptions parameter in apps/gram-hs-cli/src/GramHs/CLI/Output.hs
- [x] T027 [US1] Modify formatError to accept and pass OutputOptions parameter in apps/gram-hs-cli/src/GramHs/CLI/Output.hs
- [x] T028 [US1] Add --value-only and --deterministic flags to Parse command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Parse.hs
- [x] T029 [US1] Add --value-only and --deterministic flags to Match command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Match.hs
- [x] T030 [US1] Add --value-only and --deterministic flags to Transform command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Transform.hs
- [x] T031 [US1] Add --value-only and --deterministic flags to Generate command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T032 [US1] Add --value-only and --deterministic flags to Validate command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Validate.hs
- [x] T033 [US1] Add --value-only and --deterministic flags to Convert command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Convert.hs
- [x] T034 [US1] Run tests and verify all User Story 1 tests pass

**Checkpoint**: At this point, User Story 1 should be fully functional - developers can use --value-only and --deterministic flags with all commands

---

## Phase 4: User Story 2 - Generate Deterministic Test Suites (Priority: P1)

**Goal**: Implement `generate --type suite` command to produce deterministic test suites in the test suite format specification.

**Independent Test**: Can be fully tested by running the generate command with `--type suite` and a fixed seed, verifying that the output conforms to the test suite format specification, and confirming that the same seed produces identical test cases across multiple runs.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T035 [P] [US2] Add test for generate --type suite producing valid test suite structure in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [ ] T036 [P] [US2] Add test for deterministic test suite generation (same seed → same output) in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [ ] T037 [P] [US2] Add test for different complexity levels producing appropriate test cases in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [ ] T038 [P] [US2] Add test for test suite format schema validation in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [ ] T039 [P] [US2] Add test for count=0 producing empty test suite structure in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [ ] T040 [P] [US2] Add property test for test suite determinism across multiple runs in apps/gram-hs-cli/tests/Spec/Properties/DeterministicSpec.hs

### Test Suite Data Types

- [x] T041 [P] [US2] Define TestSuite data type with version and testCases fields in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T042 [P] [US2] Define TestCase data type with name, description, input, expected, operations fields in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T043 [P] [US2] Define TestInput data type with type and value fields in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T044 [P] [US2] Define TestExpected data type with type and value fields in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T045 [P] [US2] Define TestOperation data type with op, against, expectedBindings fields in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T046 [P] [US2] Add ToJSON instances for all test suite data types in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs

### Test Suite Generation Implementation

- [x] T047 [US2] Create generateTestSuite function that takes count, seed, complexity and returns TestSuite in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T048 [US2] Create generateTestCase function for generating individual test cases in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T049 [US2] Create testCaseName function that generates unique names (test_case_001, etc.) in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T050 [US2] Create generatePatternForComplexity function for minimal complexity level in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T051 [US2] Create generatePatternForComplexity function for basic complexity level in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T052 [US2] Create generatePatternForComplexity function for standard complexity level in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T053 [US2] Create generatePatternForComplexity function for complex complexity level in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T054 [US2] Create generatePatternForComplexity function for adversarial complexity level in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T055 [US2] Implement runGenerate to handle GenSuite type and call generateTestSuite in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T056 [US2] Add test suite JSON serialization using ToJSON instances in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T057 [US2] Add --value-only and --deterministic flags support to test suite output in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [ ] T058 [US2] Run tests and verify all User Story 2 tests pass

**Checkpoint**: At this point, User Story 2 should be fully functional - developers can generate deterministic test suites with various complexity levels

---

## Phase 5: User Story 3 - Produce Canonical JSON for Reliable Comparison (Priority: P1)

**Goal**: Add `--canonical` flag to ensure JSON keys are sorted alphabetically at all nesting levels for reliable comparison.

**Independent Test**: Can be fully tested by running commands with `--canonical` flag, verifying that JSON keys are sorted at all nesting levels, and confirming that equivalent data structures produce identical JSON output.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T059 [P] [US3] Add test for --canonical flag sorting keys at top level in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [x] T060 [P] [US3] Add test for --canonical flag sorting keys at all nesting levels in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [x] T061 [P] [US3] Add test for --canonical producing byte-for-byte identical output across runs in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [x] T062 [P] [US3] Add test for --canonical --value-only combination in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [x] T063 [P] [US3] Add test for --deterministic auto-enabling --canonical in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [ ] T064 [P] [US3] Add property test for canonical JSON determinism in apps/gram-hs-cli/tests/Spec/Properties/DeterministicSpec.hs

### Implementation for User Story 3

- [x] T065 [US3] Integrate canonicalizeJSON function into patternToJSON when canonical=True in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T066 [US3] Integrate canonicalizeJSON function into errorToJSON when canonical=True in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T067 [US3] Ensure canonicalizeJSON is applied before final encoding in apps/gram-hs-cli/src/GramHs/CLI/JSON.hs
- [x] T068 [US3] Add --canonical flag to Parse command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Parse.hs
- [x] T069 [US3] Add --canonical flag to Match command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Match.hs
- [x] T070 [US3] Add --canonical flag to Transform command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Transform.hs
- [x] T071 [US3] Add --canonical flag to Generate command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Generate.hs
- [x] T072 [US3] Add --canonical flag to Validate command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Validate.hs
- [x] T073 [US3] Add --canonical flag to Convert command in apps/gram-hs-cli/src/GramHs/CLI/Commands/Convert.hs
- [x] T074 [US3] Verify enforceDeterministicCanonical is called when processing OutputOptions in apps/gram-hs-cli/src/GramHs/CLI/Types.hs
- [ ] T075 [US3] Run tests and verify all User Story 3 tests pass

**Checkpoint**: At this point, User Story 3 should be fully functional - developers can use --canonical flag with all commands for reliable comparison

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Final integration, documentation, and edge case handling

### Documentation

- [x] T076 [P] Update manpage with --value-only flag documentation in apps/gram-hs-cli/man/gram-hs.1
- [x] T077 [P] Update manpage with --deterministic flag documentation in apps/gram-hs-cli/man/gram-hs.1
- [x] T078 [P] Update manpage with --canonical flag documentation in apps/gram-hs-cli/man/gram-hs.1
- [x] T079 [P] Update manpage with generate --type suite command documentation in apps/gram-hs-cli/man/gram-hs.1
- [x] T080 [P] Add usage examples to README.md in apps/gram-hs-cli/README.md

### Edge Cases & Error Handling

- [x] T081 [P] Add test for --value-only with command producing no result (empty object) in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T082 [P] Add test for test suite generation with invalid seed (graceful error) in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs
- [x] T083 [P] Add test for --canonical with malformed data (error handling) in apps/gram-hs-cli/tests/Spec/CLI/JSONSpec.hs
- [x] T084 [P] Add test for invalid complexity level (error handling) in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs

### Integration Tests

- [x] T085 [P] Add integration test for all flags combined (--value-only --deterministic --canonical) in apps/gram-hs-cli/tests/Spec/CLI/OutputSpec.hs
- [x] T086 [P] Add integration test for test suite generation with all complexity levels in apps/gram-hs-cli/tests/Spec/CLI/GenerateSuiteSpec.hs

### Final Validation

- [x] T087 Run full test suite and verify all tests pass
- [x] T088 Verify all functional requirements from spec.md are met
- [x] T089 Verify all success criteria from spec.md are met

---

## Dependencies & Story Completion Order

### Story Dependencies

All three user stories (US1, US2, US3) can be implemented in parallel after Phase 2 (Foundational) is complete, as they share the foundational infrastructure but don't depend on each other.

**Recommended Order** (for sequential implementation):
1. **Phase 2**: Foundational (OutputOptions, canonical JSON function, shared parser) - **MUST complete first**
2. **Phase 3**: User Story 1 (--value-only, --deterministic) - Can start immediately after Phase 2
3. **Phase 4**: User Story 2 (generate --type suite) - Can start immediately after Phase 2
4. **Phase 5**: User Story 3 (--canonical) - Can start immediately after Phase 2
5. **Phase 6**: Polish & Cross-Cutting - After all user stories complete

**Parallel Opportunities**:
- US1, US2, and US3 can be implemented in parallel after Phase 2
- Test writing can be parallelized with implementation within each story
- Documentation tasks (T076-T080) can be done in parallel

### MVP Scope

**Minimum Viable Product**: Phase 2 (Foundational) + Phase 3 (User Story 1)

This delivers the core capability for equivalence checking with `--value-only` and `--deterministic` flags, which is the most fundamental need for porting validation.

---

## Implementation Strategy

### MVP First Approach

1. **Phase 2**: Build foundational infrastructure (OutputOptions, canonical JSON, shared parser)
2. **Phase 3**: Implement User Story 1 (--value-only, --deterministic) - This is the MVP
3. **Incremental Delivery**: Add User Stories 2 and 3 as separate increments
4. **Final Polish**: Documentation, edge cases, integration tests

### Incremental Delivery

Each user story phase is independently testable and can be delivered separately:
- **Increment 1**: Foundational + US1 (MVP)
- **Increment 2**: US2 (Test suite generation)
- **Increment 3**: US3 (Canonical JSON)
- **Increment 4**: Polish & documentation

### Parallel Execution Examples

**Within User Story 1**:
- T015-T020 (tests) can be written in parallel
- T021-T027 (JSON/Output modifications) can be done in parallel with test writing
- T028-T033 (command flag additions) can be done in parallel

**Within User Story 2**:
- T035-T040 (tests) can be written in parallel
- T041-T046 (data types) can be done in parallel
- T047-T057 (generation logic) can be done incrementally

**Within User Story 3**:
- T059-T064 (tests) can be written in parallel
- T065-T073 (implementation) can be done incrementally

---

## Notes

- All tasks follow the strict checklist format: `- [ ] [TaskID] [P?] [Story?] Description with file path`
- Tests are written first (TDD approach) within each user story phase
- Each user story is independently testable and can be delivered separately
- Total task count: 89 tasks across 6 phases

