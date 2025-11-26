# Implementation Plan: Gram Parsing Conformance

**Branch**: `016-gram-parsing-conformance` | **Date**: 2025-11-25 | **Spec**: [specs/016-gram-parsing-conformance/spec.md](../spec.md)
**Input**: Feature specification from `/specs/016-gram-parsing-conformance/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement full parsing compliance for the Gram language by integrating the standard `tree-sitter-gram` test corpus. This ensures the Haskell parser supports all valid syntax, including complex path expressions (e.g., `(a)-->(b)-->(c)`), and correctly reports errors for invalid inputs.

## Technical Context

**Language/Version**: Haskell (GHC 9.x)
**Primary Dependencies**: `megaparsec` (parsing), `hspec` (testing)
**Storage**: N/A (In-memory data structures)
**Testing**: `hspec` for unit tests, custom runner for `tree-sitter-gram` corpus
**Target Platform**: Cross-platform (library)
**Project Type**: Library
**Performance Goals**: N/A
**Constraints**: Must not require `tree-sitter` runtime dependency (pure Haskell implementation)
**Scale/Scope**: ~25 corpus files, hundreds of test cases

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: Parsing logic is already in place; new code will focus on test integration and gap filling.
- **Testing Standards (NON-NEGOTIABLE)**: Integrating the authoritative corpus ensures the highest standard of testing.
- **Conceptual Consistency**: The `Pattern` structure must accurately reflect the Gram data model.
- **Mathematical Clarity**: N/A (Parsing focus)
- **Multi-Language Reference Alignment**: Ensuring the Haskell implementation matches the canonical `tree-sitter` definition aligns with this principle.

**Violations must be documented in Complexity Tracking section below.**

## Project Structure

### Documentation (this feature)

```text
specs/016-gram-parsing-conformance/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
└── tasks.md             # Phase 2 output
```

### Source Code (repository root)

```text
libs/gram/
├── src/
│   └── Gram/
│       ├── Parse.hs     # Parser implementation
│       └── Test.hs      # New module for corpus test runner (or integration in existing test)
├── tests/
│   └── Spec/
│       └── CorpusSpec.hs # New spec for corpus tests
└── test-data/
    └── tree-sitter-gram/ # Existing submodule
```

**Structure Decision**: Integrate corpus tests into `libs/gram/tests/` using a new Spec file `CorpusSpec.hs`.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| | | |
