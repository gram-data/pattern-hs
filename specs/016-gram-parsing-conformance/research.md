# Research: Gram Parsing Conformance

**Feature**: Gram Parsing Conformance
**Date**: 2025-11-25

## Research Goals

1.  Determine how to effectively integrate `tree-sitter-gram` corpus tests into the Haskell test suite.
2.  Analyze the gap between current `megaparsec` implementation and `tree-sitter-gram` syntax expectations, specifically regarding "Path" syntax.

## Findings

### 1. Corpus Test Integration

The `tree-sitter-gram` corpus is located in `libs/gram/test-data/tree-sitter-gram/test/corpus/`.
These files follow a specific format:

```text
==================
Test Name
[:error]  <-- Optional tag indicating expected failure
==================
Input Code
---
Expected S-Expression
```

**Decision**:
We will implement a Haskell-based test runner (likely in `tests/Spec/CorpusSpec.hs`) that:
1.  Reads all `.txt` files from the corpus directory.
2.  Parses each file to extract individual test cases (Name, Input, Expected Success/Failure).
3.  Executes `Gram.Parse.fromGram` on the Input.
4.  Asserts:
    - If `:error` tag is present: Parser returns `Left`.
    - If no tag: Parser returns `Right`.
    - (Optional Future): Compare resulting structure against S-Expression (out of scope for initial pass due to complexity).

**Rationale**:
Directly using the corpus ensures 100% alignment with the standard. Implementing a custom runner avoids adding a dependency on the `tree-sitter` runtime/bindings, keeping the Haskell library pure.

### 2. Path Syntax Gap Analysis

The "Path" syntax in Gram usually refers to sequences like `(a)-->(b)-->(c)`.
The current `Gram.Parse.hs` implements `parseRelationship` as `node + kind + path`.
`parsePath` is `relationship <|> node`.

This recursive definition `(a)-->(b)-->(c)` parses as:
- `(a)` (Left)
- `-->` (Kind)
- `(b)-->(c)` (Right, parsed recursively)

This produces a nested structure: `Pattern a [Pattern b [Pattern c []]]`.

**Potential Gap**:
1.  **Parsing Logic**: The recursion in `parseRelationship` combined with `parsePath` relies on specific operator precedence or greedy matching. If `parseNode` consumes `(b)` before `parseRelationship` sees the second `-->`, the parse might fail or stop early.
2.  **Data Model**: If the intent of "Path" is a flat list of edges, the nested `Pattern` structure is technically a tree. However, Gram's pattern model *is* recursive/nested. We need to verify if this nesting matches the semantic expectation.
3.  **Missing Features**: Top-level paths (not inside `[]`) might need specific handling in `fromGram`. Currently `fromGram` expects `optional(record) + repeat(pattern)`. `pattern` is `commaSep1(pattern_element)`. `pattern_element` is `subject | path`.
    - If `(a)-->(b)` is passed, it matches `path` -> `relationship`.
    - If `(a)-->(b), (c)` is passed, it matches `pattern` with two elements.

**Hypothesis**:
The "Path" gap might be related to complex paths involving multiple hops or specific edge cases in the recursion logic (e.g. backtracking issues). The corpus tests will reveal exactly which path constructs fail.

### 3. Integration Strategy

We will proceed by:
1.  Implementing the `CorpusSpec` runner.
2.  Running it against the *entire* corpus.
3.  Analyzing failures to identify specific syntax gaps.
4.  Fixing `Gram.Parse.hs` to make tests pass.

## Alternatives Considered

-   **Using `tree-sitter` bindings**: Rejected. Adds C dependency and runtime complexity. We want a pure Haskell parser.
-   **Manual Test Porting**: Rejected. Too maintenance heavy. We want to stay in sync with the standard corpus.

## Risks

-   **Structure Mismatch**: We verify *syntax validity* (it parses), but not *structural correctness* (it parses into the *right* thing) against the corpus S-expressions.
-   **Mitigation**: We assume that if it parses successfully and passes our existing unit tests (which check structure), it is likely correct. We can add specific structural tests for complex paths if needed.

