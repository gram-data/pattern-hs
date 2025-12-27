# gram-hs CLI

The `gram-hs` CLI serves as the reference implementation and conformance testing tool for the gram/pattern ecosystem. It provides canonical outputs for pattern operations, generates test suites, and enables validation of ports to other languages.

## Building

From the project root:

```bash
cabal build gram-hs-cli
```

Or to install:

```bash
cabal install gram-hs-cli
```

## Commands

### Parse

Parse gram notation and output canonical representation:

```bash
gram-hs parse input.gram [--format json|gram|debug]
```

Reads from stdin if no file is provided:

```bash
cat input.gram | gram-hs parse
```

### Match

Execute pattern matching and output bindings:

```bash
gram-hs match <pattern-file> <data-file> [--format json|table|count]
```

### Transform

Apply pattern transformations:

```bash
gram-hs transform <operation> <input-file> [--format json|gram|debug]
```

Operations: `fold`, `map`, `filter`, `reverse`, `flatten`, `normalize`

### Generate

Generate test data and patterns:

```bash
gram-hs generate <generator-type> [--count N] [--seed S] [--complexity C]
```

Generator types: `pattern`, `graph`, `suite`, `property`

### Validate

Run conformance test suites:

```bash
gram-hs validate <test-suite> [--runner external-command]
```

### Convert

Convert between different representations:

```bash
gram-hs convert <input-file> --from <format> --to <format>
```

Supported formats: `gram`, `json`, `cypher`, `dot`, `mermaid`

## Output Formats

### JSON (Canonical)

All JSON output follows strict rules for determinism with metadata including version, command, timestamp, and hash.

### Gram

Pretty-printed gram notation.

### Debug

Haskell Show instance output for debugging.

## Exit Codes

- `0` - Success
- `1` - Parse error
- `2` - Validation failure
- `3` - Invalid arguments
- `4` - IO error
- `5` - Timeout/resource limit

