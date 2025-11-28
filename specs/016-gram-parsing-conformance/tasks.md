# Tasks: Gram Parsing Conformance

**Feature Branch**: `016-gram-parsing-conformance`
**Feature Name**: Gram Parsing Conformance

## Phase 1: Setup
- [x] T001 Initialize `tree-sitter-gram` submodule in `libs/gram/test-data/tree-sitter-gram`

## Phase 2: Foundation
- [x] T002 Create `libs/gram/tests/Spec/CorpusSpec.hs` with module shell
- [x] T003 Implement corpus file parsing logic in `libs/gram/tests/Spec/CorpusSpec.hs` to extract test cases (Name, Input, Expected)
- [x] T004 Implement Hspec test generation in `libs/gram/tests/Spec/CorpusSpec.hs` to iterate over all corpus files
- [x] T005 Register `Spec.CorpusSpec` in `libs/gram/tests/Test.hs`

## Phase 3: Full Syntax Compliance (P1)
**Goal**: Ensure parser handles all valid syntax defined in the standard Gram specification.
**Independent Test**: Run `cabal test` and verify zero failures for positive corpus cases.

- [x] T006 [US1] Run initial corpus tests to identify syntax gaps and establish baseline failures
- [x] T007 [US1] Analyze "Path" syntax failures and determine recursion/backtracking issues in `Gram.Parse`
- [x] T008 [US1] Fix `parseRelationship` and `parsePath` in `libs/gram/src/Gram/Parse.hs` to support chained paths (e.g., `(a)-->(b)-->(c)`)
- [x] T009 [US1] Address missing top-level syntax support (if any) in `fromGram` in `libs/gram/src/Gram/Parse.hs`
- [x] T010 [US1] Verify all positive corpus test cases pass

## Phase 4: Parse Error Reporting (P2)
**Goal**: Ensure parser correctly rejects invalid syntax as defined by the corpus.
**Independent Test**: Run `cabal test` and verify zero failures for negative corpus cases (marked with `:error`).

- [x] T011 [US2] Update `libs/gram/tests/Spec/CorpusSpec.hs` to handle `:error` tags in test cases
- [x] T012 [US2] Implement assertion logic to verify parser returns `Left` for `:error` inputs
- [x] T013 [US2] Verify all negative corpus test cases pass (i.e., parsing fails as expected)
- [x] T014 [US2] [P] Improve error context in `libs/gram/src/Gram/Parse.hs` if valid inputs are being rejected or invalid inputs accepted incorrectly

## Phase 5: Path Semantics Correction
**Goal**: Correctly map Path syntax `(a)-[r]->(b)` to Edge Pattern `[r | (a), (b)]` instead of nesting.

- [x] T015 [US1] Modify `Gram.Parse.parseRelationship` to produce `Pattern r [left, right]` instead of `Pattern left [right]`
- [x] T016 [US1] Update `Gram.Parse.parsePath` to handle the non-nested chain structure
- [x] T017 [US1] Verify all corpus tests still pass with new structure
- [x] T018 [US1] Add specific structural test case in `ParseSpec.hs` verifying `(a)-[r]->(b)` maps to `[r | (a), (b)]`

## Phase 6: Polish
- [x] T019 Run full test suite `cabal test` to ensure no regressions in existing tests
- [x] T020 Document syntax support status and any known deviations in `libs/gram/SYNTAX_NOTES.md`

## Dependencies

1. **T001-T005** (Setup/Foundation) must be completed first to enable testing.
2. **T006** (Baseline) informs the fixes in **T007-T009**.
3. **T011-T013** (Error Reporting) can be done in parallel with **T008-T010** (Positive Compliance) once T004 is done, but logically follow syntax fixes.
4. **T015-T018** (Path Semantics) requires the basic parsing to be working first.

## Parallel Execution Examples

- **US1 Fixes**: While one developer fixes Path syntax (T008), another can investigate other syntax gaps (T009).
- **Test Runner vs Parser**: T003/T004 (Runner) can be built while T008 (Parser Fix) is being researched.

## Implementation Strategy

1. **Foundation First**: We need the test harness working to see what's broken.
2. **Green Path**: Focus on getting valid syntax (positive tests) passing first.
3. **Red Path**: Ensure invalid syntax is rejected.
4. **Structural Correctness**: Fix the semantic mapping of paths.
