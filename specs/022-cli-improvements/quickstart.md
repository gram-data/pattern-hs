# Quick Start: CLI Tool Improvements for Language Porting

**Feature**: 022-cli-improvements  
**Date**: 2025-12-27

## Overview

This feature adds three key improvements to the gram-hs CLI tool to make it easier to port the project to other languages:

1. **Metadata Exclusion** (`--value-only`, `--deterministic`) - Compare outputs without changing metadata
2. **Test Suite Generation** (`generate --type suite`) - Generate deterministic test cases
3. **Canonical JSON** (`--canonical`) - Reliable comparison with sorted keys

## Quick Examples

### Compare Outputs Without Metadata

```bash
# Output only the pattern value (no metadata)
gram-hs parse input.gram --value-only

# Deterministic output with fixed metadata
gram-hs parse input.gram --deterministic

# Both: clean, deterministic output
gram-hs parse input.gram --value-only --deterministic
```

### Generate Test Suites

```bash
# Generate 10 test cases with seed 42
gram-hs generate --type suite --count 10 --seed 42 > test_suite.json

# Generate complex test cases
gram-hs generate --type suite --count 5 --complexity complex --seed 100
```

### Canonical JSON for Comparison

```bash
# Sorted keys at all levels
gram-hs parse input.gram --canonical

# Combine with value-only for clean comparison
gram-hs parse input.gram --value-only --canonical
```

## Common Workflows

### Equivalence Checking Between Implementations

```bash
# Generate reference output (deterministic, canonical, value-only)
gram-hs parse input.gram --value-only --canonical --deterministic > reference.json

# Compare with other implementation
other-impl parse input.gram | diff reference.json -
```

### Generate Test Suite for Port Validation

```bash
# Generate comprehensive test suite
gram-hs generate --type suite --count 100 --complexity standard --seed 42 > test_suite.json

# Use test suite to validate port implementation
# (test suite format can be consumed by validation tools)
```

### Snapshot Testing

```bash
# Generate deterministic snapshot
gram-hs parse input.gram --canonical --deterministic > snapshot.json

# Compare snapshots (should be identical)
diff snapshot1.json snapshot2.json
```

## Flag Reference

### `--value-only`
- Output only result value, no metadata wrapper
- Use for: Clean comparison output

### `--deterministic`
- Use fixed metadata values (timestamp, hash)
- Auto-enables `--canonical`
- Use for: Deterministic output across runs

### `--canonical`
- Sort JSON keys alphabetically at all levels
- Use for: Reliable text comparison

### `generate --type suite`
- Generate test suites in standard format
- Options: `--count`, `--seed`, `--complexity`
- Use for: Automated test case generation

## Flag Combinations

| Combination | Result |
|------------|--------|
| `--value-only` | Result value only, no metadata |
| `--canonical` | Sorted keys, with metadata |
| `--deterministic` | Fixed metadata, sorted keys (auto-canonical) |
| `--value-only --canonical` | Result value only, sorted keys |
| `--deterministic --value-only` | Result value only, deterministic (auto-canonical) |

## Next Steps

1. **For Porting**: Use `--value-only --canonical` to generate reference outputs
2. **For Testing**: Use `generate --type suite` to create test cases
3. **For Validation**: Use deterministic flags to ensure reproducible comparisons

## See Also

- [CLI Interface Contract](contracts/cli-interface.md) - Detailed flag specifications
- [Data Model](data-model.md) - Data structure definitions
- [Feature Specification](spec.md) - Complete feature requirements

