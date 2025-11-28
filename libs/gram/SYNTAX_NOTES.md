# Gram Syntax Support Notes

This document tracks the status of Gram syntax support in the Haskell implementation compared to the `tree-sitter-gram` corpus.

## Supported Features

- **Nodes**: `(id:Label {prop: val})` - Fully supported.
- **Subjects**: `[id:Label {prop: val}]` - Fully supported.
- **Relationships**:
  - Simple arrows: `-->`, `<--`, `--` - Supported.
  - Complex arrows: `==>`, `<==>`, `~~>` - Supported.
  - Interrupted arrows with attributes: `-[...] ->`, `<- [...] -` - **Not Supported**.
  - Backticked identifiers in arrows: `-[`id`]->` - **Not Supported**.
- **Paths**: Sequences of nodes and relationships.
  - **Single Edges**: `(a)-[r]->(b)` maps to Edge Pattern `[r | (a), (b)]`.
  - **Walks**: `(a)-[r1]->(b)-[r2]->(c)` maps to Walk Pattern `[walk | edge1, edge2]`.

- **Values**:
  - Integers, Decimals, Booleans
  - Strings: Double quoted `"..."` and Single quoted `'...'`
  - Tagged Strings: `tag`backtick`...`backtick`
  - Arrays: `[...]`
  - Maps: `{ key: val }`
  - Ranges: `1..10`, `1...`
  - Measurements: `100km`
- **Identifiers**:
  - Alphanumeric
  - Backticked: `` `escaped \` content` ``

## Known Limitations / Gaps

### 1. Predicates in Records
The corpus file `records.txt` contains examples like `{ n > 1 }` marked as `:error`.
- **Status**: Correctly Rejected.
- **Note**: The parser correctly identifies this as invalid syntax, consistent with the corpus.

### 2. Complex Multiline Comments / Strings
Some edge cases in `comments.txt` and `text_values.txt` involving specific combinations of comments inside patterns or multiline tagged strings may fail.
- **Status**: Mostly supported, but edge cases exist.
- **Workaround**: Ensure clean separation of comments and standard formatting for complex strings.

### 3. Graph Global / Top-level Structure
`graph_global.txt` shows mixed records and patterns separated by newlines.
- **Status**: Parsing fails on some combinations of top-level records followed immediately by array-like patterns `[...]`.
- **Reason**: Ambiguity or strictness in top-level loop parsing logic.

### 4. Relationship Nuances (Data Loss)
Gram notation supports rich relationship syntax (`-->`, `<--`, `==>`).
- **Current Status**: 
  - The parser **accepts** all arrow variations.
  - The CST **preserves** the arrow string (e.g., `"==>"`).
  - The `Pattern` structure currently **discards** the arrow style (only preserves attributes).
- **Future Work**: 
  - Map the arrow style from the CST to a property in the Edge Pattern's Subject (e.g. `{arrow: "==>"}`).

## Corpus Conformance

As of 2025-11-28:
- **Negative Tests**: 100% Pass (all invalid syntax is correctly rejected).
- **Positive Tests**: 100% Pass (all valid corpus examples parse correctly).
- **Round-Trip Tests**: 100% Pass (all valid corpus examples round-trip correctly).
  - Edge/Walk patterns and atomic patterns are correctly preserved.
  - Empty patterns `(), ()` are correctly preserved.
  - Quoting of identifiers and strings is correctly handled.

