# Research: CLI Tool Improvements for Language Porting

**Feature**: 022-cli-improvements  
**Date**: 2025-12-27  
**Status**: Complete

## Research Questions

### 1. Test Suite Format Specification

**Question**: What is the exact schema for the test suite format that `generate --type suite` must produce?

**Research**: The design document references `specs/002-workspace-setup/contracts/test-sync-format.md`, but this file was not found in the codebase. However, the design document provides a clear example format:

```json
{
  "version": "1.0",
  "test_cases": [
    {
      "name": "test_case_identifier",
      "description": "Human-readable description",
      "input": {
        "type": "gram_notation",
        "value": "(node)-[edge]->(target)"
      },
      "expected": {
        "type": "pattern",
        "value": { ... }
      },
      "operations": [
        {
          "op": "match",
          "against": "(pattern)",
          "expected_bindings": [ ... ]
        }
      ]
    }
  ]
}
```

**Decision**: Use the format specified in the design document as the canonical schema. The test suite will include:
- Top-level `version` field (string, "1.0")
- `test_cases` array containing test case objects
- Each test case has: `name`, `description`, `input`, `expected`, and optional `operations` array

**Rationale**: The design document provides sufficient detail to implement the format. If a formal specification file exists later, we can validate against it, but the documented format is clear and unambiguous.

**Alternatives Considered**: 
- Waiting for formal specification file: Rejected because it blocks implementation and the design document format is sufficient
- Creating a new specification: Rejected because the design document already defines the format clearly

---

### 2. Canonical JSON with Sorted Keys in Haskell

**Question**: How to ensure JSON keys are sorted alphabetically at all nesting levels using aeson?

**Research**: The `aeson` library provides `encode` and `encodePretty` functions. By default, `encodePretty` uses `Config` with `confCompare` for key ordering. However, `encodePretty` doesn't guarantee sorted keys at all nesting levels by default.

**Decision**: Use a custom `ToJSON` instance or post-processing approach:
1. Create a custom JSON encoding function that recursively sorts keys
2. Use `aeson`'s `Value` type to traverse and sort all object keys
3. Apply sorting before final encoding

**Implementation Approach**:
- Create a function `canonicalizeJSON :: Value -> Value` that recursively sorts all object keys
- Apply this function to the JSON `Value` before encoding
- Use `encode` (not `encodePretty`) after canonicalization to avoid double-formatting

**Rationale**: This approach ensures deterministic key ordering at all levels while maintaining compatibility with existing aeson infrastructure. The recursive sorting is straightforward to implement and test.

**Alternatives Considered**:
- Using `encodePretty` with custom `Config`: Rejected because it only affects top-level keys
- External JSON processing: Rejected because it adds unnecessary dependencies and complexity
- Custom `ToJSON` instances for every type: Rejected because it's too invasive and doesn't guarantee nested sorting

---

### 3. Deterministic Metadata Values

**Question**: How to handle metadata (timestamps, hashes) when `--deterministic` flag is used?

**Research**: Current implementation generates timestamps using `getCurrentTime` and hashes using SHA256 of the JSON output. For deterministic output, these must be fixed or excluded.

**Decision**: 
- When `--deterministic` is used:
  - Use fixed timestamp: `"1970-01-01T00:00:00+0000"` (Unix epoch)
  - Use fixed hash: `"0000000000000000000000000000000000000000000000000000000000000000"` (64 zero hex chars)
  - Or exclude metadata entirely when combined with `--value-only`

**Rationale**: Fixed values provide deterministic output while maintaining the JSON structure. Using epoch timestamp and zero hash are clear indicators of deterministic mode. When `--value-only` is also used, metadata is excluded entirely.

**Alternatives Considered**:
- Excluding metadata always with `--deterministic`: Rejected because some users may want deterministic output with metadata structure
- Using seed-based deterministic values: Rejected because it adds unnecessary complexity
- Empty strings: Rejected because fixed recognizable values are clearer

---

### 4. Output Options Architecture

**Question**: How to structure the output options (`--value-only`, `--deterministic`, `--canonical`) across all commands?

**Research**: Current CLI structure uses `OutputFormat` type and passes it to `formatOutput` function. Commands parse their own options independently.

**Decision**: 
1. Extend `OutputFormat` type to include output options:
   ```haskell
   data OutputOptions = OutputOptions
     { optValueOnly :: Bool
     , optDeterministic :: Bool
     , optCanonical :: Bool
     }
   ```
2. Create a shared parser for output flags that all commands can use
3. Pass `OutputOptions` alongside `OutputFormat` to output functions
4. When `--deterministic` is used, automatically enable `--canonical` (as per FR-011)

**Rationale**: This approach:
- Allows flags to be combined (as required by FR-003, FR-010)
- Centralizes output logic in one place
- Makes it easy to add flags to all commands

**Alternatives Considered**:
- Separate types per command: Rejected because it duplicates code and makes flag combination harder
- Global config file: Rejected because it adds complexity and the spec requires CLI flags
- Environment variables only: Rejected because spec explicitly requires CLI flags

---

### 5. Test Suite Generation Strategy

**Question**: How to generate diverse, deterministic test cases covering various pattern structures and complexity levels?

**Research**: Current `generatePatterns` function is very simple and only generates basic patterns. Test suite generation needs to cover:
- Various pattern structures (atomic, nested, complex)
- Different complexity levels (minimal, basic, standard, complex, adversarial)
- Deterministic generation based on seed

**Decision**:
1. Use `System.Random` with `mkStdGen` seeded from `--seed` parameter
2. Create generator functions for each complexity level:
   - `minimal`: Simple atomic patterns
   - `basic`: Patterns with 1-3 elements
   - `standard`: Patterns with nested structures, 3-10 elements
   - `complex`: Deep nesting, 10-50 elements, various edge cases
   - `adversarial`: Stress cases, maximum nesting, unusual structures
3. Generate test cases deterministically by iterating through seed values
4. Each test case includes: name (based on index), description, input (gram notation), expected (parsed pattern), and optional operations

**Rationale**: This approach provides:
- Deterministic output (same seed → same test cases)
- Coverage across complexity levels
- Scalable generation (can generate 1-1000+ test cases)
- Clear mapping from seed to test case sequence

**Alternatives Considered**:
- Pre-defined test case library: Rejected because it doesn't scale and isn't deterministic based on seed
- External test case files: Rejected because it doesn't meet the requirement for programmatic generation
- Template-based generation: Rejected because it's less flexible than direct generation

---

## Summary

All research questions have been resolved with clear decisions that:
- Support deterministic output
- Enable reliable comparison across implementations
- Follow Haskell/aeson best practices
- Meet all functional requirements from the specification

No blocking issues identified. Implementation can proceed.

