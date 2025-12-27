# CLI Interface Contract: Output Formatting Flags

**Feature**: 022-cli-improvements  
**Date**: 2025-12-27  
**Version**: 1.0

## Overview

This contract defines the CLI interface extensions for output formatting flags that enable deterministic, comparable output for language porting validation.

## Global Output Flags

These flags can be used with any command that produces JSON output.

### `--value-only`

**Purpose**: Output only the result value without metadata wrapper.

**Usage**: `gram-hs <command> [args...] --value-only`

**Behavior**:
- When used with successful commands: Output contains only the pattern/result JSON value
- When used with error conditions: Output contains only the error object
- Metadata fields (`Meta`, `Result`, `Diagnostics`) are excluded
- Compatible with `--canonical` flag

**Example**:
```bash
# Without --value-only
$ gram-hs parse input.gram
{
  "Meta": { "Version": "0.1.0", "Command": "parse", ... },
  "Result": { "Type": "Pattern", "Value": { ... } }
}

# With --value-only
$ gram-hs parse input.gram --value-only
{
  "value": { ... },
  "elements": [ ... ]
}
```

**Exit Codes**: Same as base command (no change)

---

### `--deterministic`

**Purpose**: Ensure deterministic output by using fixed values for metadata or excluding it.

**Usage**: `gram-hs <command> [args...] --deterministic`

**Behavior**:
- When used: All metadata uses fixed values:
  - Timestamp: `"1970-01-01T00:00:00+0000"`
  - Hash: `"0000000000000000000000000000000000000000000000000000000000000000"`
- Automatically enables `--canonical` (sorted keys)
- Compatible with `--value-only` (metadata excluded entirely)

**Example**:
```bash
# Without --deterministic (changing timestamp/hash each run)
$ gram-hs parse input.gram
{ "Meta": { "Timestamp": "2025-12-27T10:30:45+0000", "Hash": "abc123..." }, ... }

# With --deterministic (fixed values)
$ gram-hs parse input.gram --deterministic
{ "Meta": { "Timestamp": "1970-01-01T00:00:00+0000", "Hash": "0000..." }, ... }
```

**Exit Codes**: Same as base command (no change)

---

### `--canonical`

**Purpose**: Produce JSON with sorted keys at all nesting levels for reliable comparison.

**Usage**: `gram-hs <command> [args...] --canonical`

**Behavior**:
- All object keys sorted alphabetically at every nesting level
- Consistent formatting (no optional whitespace)
- Deterministic across runs for equivalent data structures
- Automatically enabled when `--deterministic` is used

**Example**:
```bash
# Without --canonical (keys may be in any order)
$ gram-hs parse input.gram
{ "elements": [...], "value": {...} }

# With --canonical (keys sorted)
$ gram-hs parse input.gram --canonical
{ "elements": [...], "value": {...} }  # Same order every time
```

**Exit Codes**: Same as base command (no change)

---

## Flag Combinations

### `--value-only --canonical`

**Behavior**: Output only result value with sorted keys.

**Use Case**: Clean, comparable output for equivalence checking.

**Example**:
```bash
$ gram-hs parse input.gram --value-only --canonical
{ "elements": [], "value": { ... } }  # Sorted keys, no metadata
```

---

### `--deterministic --value-only`

**Behavior**: Output only result value (metadata excluded entirely, deterministic).

**Use Case**: Maximum determinism with minimal output.

**Example**:
```bash
$ gram-hs parse input.gram --deterministic --value-only
{ "elements": [], "value": { ... } }  # No metadata, canonical (auto-enabled)
```

---

### `--deterministic --canonical`

**Behavior**: Deterministic metadata with sorted keys (canonical is auto-enabled).

**Use Case**: Deterministic output with metadata structure preserved.

**Example**:
```bash
$ gram-hs parse input.gram --deterministic --canonical
{ "Meta": { ... }, "Result": { ... } }  # Fixed metadata, sorted keys
```

---

## Generate Command Extensions

### `generate --type suite`

**Purpose**: Generate test suites in the test suite format specification.

**Usage**: `gram-hs generate --type suite [--count N] [--seed S] [--complexity LEVEL] [--format json]`

**Required Flags**:
- `--type suite` - Specifies test suite generation

**Optional Flags**:
- `--count N` - Number of test cases to generate (default: 1)
- `--seed S` - Random seed for deterministic generation (default: 42)
- `--complexity LEVEL` - Complexity level: `minimal`, `basic`, `standard`, `complex`, `adversarial` (default: `basic`)
- `--format json` - Output format (default: json, only format supported for test suites)

**Output Format**: JSON conforming to test suite format specification:
```json
{
  "version": "1.0",
  "test_cases": [
    {
      "name": "test_case_001",
      "description": "Human-readable description",
      "input": {
        "type": "gram_notation",
        "value": "(node)-[edge]->(target)"
      },
      "expected": {
        "type": "pattern",
        "value": { ... }
      },
      "operations": [ ... ]
    }
  ]
}
```

**Determinism**: Same seed produces identical test suite output.

**Example**:
```bash
# Generate 10 test cases with seed 42
$ gram-hs generate --type suite --count 10 --seed 42

# Generate 5 complex test cases
$ gram-hs generate --type suite --count 5 --complexity complex --seed 100
```

**Exit Codes**:
- `0` - Success
- `3` - Invalid arguments (invalid complexity level, negative count, etc.)

---

## Error Handling

### Invalid Flag Combinations

**Behavior**: Flags are designed to be compatible. No invalid combinations exist.

### Invalid Arguments

**Behavior**: 
- Invalid complexity level → Error message, exit code 3
- Negative count → Error message, exit code 3
- Invalid seed format → Error message, exit code 3

**Error Output Format** (when `--value-only` is used):
```json
{
  "type": "ArgumentError",
  "message": "Invalid complexity level: 'invalid'"
}
```

**Error Output Format** (default, with metadata):
```json
{
  "Error": {
    "type": "ArgumentError",
    "message": "Invalid complexity level: 'invalid'",
    ...
  }
}
```

---

## Output Format Specifications

### Value-Only Output

**Structure**: Direct pattern/result JSON without wrapper:
```json
{
  "value": { ... },
  "elements": [ ... ]
}
```

**Error Format** (value-only):
```json
{
  "type": "ParseError",
  "message": "Error message",
  ...
}
```

### Canonical JSON

**Key Ordering**: All object keys sorted alphabetically at all nesting levels.

**Formatting**: Consistent, no optional whitespace, standard JSON encoding.

### Deterministic Metadata

**Fixed Values**:
- `Timestamp`: `"1970-01-01T00:00:00+0000"`
- `Hash`: `"0000000000000000000000000000000000000000000000000000000000000000"`
- `Version`: `"0.1.0"` (unchanged)
- `Command`: Command name (unchanged)

---

## Validation

### Test Suite Format Validation

Generated test suites must:
- Include `version` field with value "1.0"
- Include `test_cases` array
- Each test case must have: `name`, `description`, `input`, `expected`
- `input.value` must be valid gram notation
- `expected.value` must be valid pattern JSON
- All test case names must be unique within the suite

### Determinism Validation

- Same seed + same count + same complexity → identical output
- Same input + same flags → identical output (when deterministic flags used)

---

## Notes

- All flags can be combined where it makes sense
- Flags apply to JSON output format only (not gram or debug formats)
- Test suite generation only supports JSON format
- Exit codes follow existing CLI conventions

