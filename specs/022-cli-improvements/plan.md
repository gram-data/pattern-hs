# Implementation Plan: CLI Tool Improvements for Language Porting

**Branch**: `022-cli-improvements` | **Date**: 2025-12-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/022-cli-improvements/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

This feature adds three high-priority CLI improvements to the gram-hs reference implementation tool to facilitate porting to other languages:

1. **Metadata Exclusion**: Add `--value-only` and `--deterministic` flags to exclude or fix metadata (timestamps, hashes) for reliable equivalence checking
2. **Test Suite Generation**: Implement `generate --type suite` command to produce deterministic test suites in the test suite format specification
3. **Canonical JSON Output**: Add `--canonical` flag to ensure sorted keys and consistent formatting for byte-for-byte identical output

These improvements enable automated testing and validation workflows when porting gram-hs to other languages by providing deterministic, comparable outputs.

## Technical Context

**Language/Version**: Haskell (GHC 4.17.0.0+)  
**Primary Dependencies**: 
- `optparse-applicative ^>=0.18` - CLI argument parsing
- `aeson ^>=2.2` - JSON serialization
- `aeson-pretty ^>=0.8` - JSON pretty-printing
- `text >=2.0` - Text handling
- `time ^>=1.12` - Timestamp generation
- `cryptohash-sha256 ^>=0.11` - Hash computation
- `random ^>=1.2` - Deterministic random generation for test suites

**Storage**: N/A (CLI tool, no persistent storage)  
**Testing**: 
- Haskell test frameworks (HUnit, QuickCheck, Tasty)
- Property-based testing for deterministic generation
- Integration tests for CLI commands

**Target Platform**: Linux/macOS/Windows (cross-platform CLI)  
**Project Type**: Single executable CLI application  
**Performance Goals**: 
- JSON serialization should complete in <100ms for typical patterns
- Test suite generation should produce 100 test cases in <5 seconds
- CLI startup and command execution should be <500ms for typical operations

**Constraints**: 
- Output must be deterministic when using `--deterministic` or `--canonical`
- Test suite output must conform to test suite format specification schema
- JSON key sorting must work at all nesting levels

**Scale/Scope**: 
- CLI tool used by developers porting to other languages
- Test suite generation: 1-1000 test cases per run
- Output comparison: typically 1-100 comparisons per validation run

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Mandatory Compliance Checks:**

- **Code Quality (NON-NEGOTIABLE)**: ✅ The design provides clear, self-documenting code structure through modular command organization. All new flags and functions will be documented with examples in the manpage and inline documentation. JSON serialization logic is already well-structured in `GramHs.CLI.JSON` module.

- **Testing Standards (NON-NEGOTIABLE)**: ✅ The testing strategy is comprehensive:
  - Unit tests for each new flag combination
  - Property-based tests for deterministic output (same seed → same output)
  - Integration tests for CLI command execution with new flags
  - Schema validation tests for test suite format conformance
  - Edge case tests for error conditions and boundary values

- **Conceptual Consistency**: ✅ This feature does not modify core pattern/gram data structures or category-theoretic properties. It only affects CLI output formatting and test generation, maintaining conceptual consistency with existing design.

- **Mathematical Clarity**: ✅ Deterministic output requirements are clearly specified (same seed → identical output). Canonical JSON sorting is mathematically well-defined (alphabetical key ordering at all levels).

- **Multi-Language Reference Alignment**: ✅ The improvements directly support multi-language porting:
  - Deterministic output enables reliable comparison across implementations
  - Test suite format provides standardized test cases for all language ports
  - Canonical JSON ensures consistent serialization across implementations

**Violations**: None identified. All principles are satisfied.

## Project Structure

### Documentation (this feature)

```text
specs/022-cli-improvements/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
apps/gram-hs-cli/
├── src/
│   ├── Main.hs
│   └── GramHs/
│       ├── CLI.hs                    # Main CLI dispatcher
│       ├── CLI/
│       │   ├── Types.hs             # OutputFormat, OutputOptions (NEW)
│       │   ├── Output.hs             # formatOutput, formatError (MODIFY)
│       │   ├── JSON.hs               # JSON serialization (MODIFY)
│       │   └── Commands/
│       │       ├── Parse.hs          # Add --value-only, --deterministic, --canonical flags
│       │       ├── Match.hs          # Add output flags
│       │       ├── Transform.hs      # Add output flags
│       │       ├── Generate.hs       # Implement --type suite, add output flags
│       │       ├── Validate.hs       # Add output flags
│       │       └── Convert.hs        # Add output flags
│
├── tests/
│   └── Spec/
│       ├── CLI/
│       │   ├── OutputSpec.hs         # Tests for output formatting
│       │   ├── JSONSpec.hs            # Tests for canonical JSON
│       │   └── GenerateSuiteSpec.hs   # Tests for test suite generation
│       └── Properties/
│           └── DeterministicSpec.hs  # Property tests for determinism
│
└── man/
    └── gram-hs.1                      # Update manpage with new flags
```

**Structure Decision**: Single executable CLI application. All changes are within the existing `apps/gram-hs-cli` directory. New functionality is added through:
- Extended `OutputFormat` type to include output options (value-only, deterministic, canonical)
- Modified JSON serialization to support these options
- Enhanced Generate command to implement test suite generation
- Comprehensive test coverage for all new features

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations identified. All complexity is justified by the requirement to support multi-language porting through deterministic, comparable outputs.
