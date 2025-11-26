# Data Model: Gram Parsing Conformance

**Feature**: Gram Parsing Conformance
**Status**: Stable

## Core Entities

The data model remains consistent with the existing `Gram` library structure. No new types are introduced, but we clarify how "Paths" are represented.

### Pattern

The core structure returned by the parser.

```haskell
data Pattern a = Pattern
  { value    :: a           -- ^ The head/subject of the pattern
  , elements :: [Pattern a] -- ^ Nested elements (children, path tails, etc.)
  }
```

### Path Representation

A path `(a)-[r1]->(b)-[r2]->(c)` is represented as a nested `Pattern` structure:

1.  **Head**: `(a)` (Node)
2.  **Elements**: Contains one element: the rest of the path starting from `(b)`.
    - The relationship `-[r1]->` information is currently *implicit* in the structure or lost if not stored.
    - **Note**: The current `Pattern` structure does *not* store the relationship type/kind (e.g., `-->`, `<--`) in the generic `Pattern` wrapper. It stores the *nodes*.
    - If the `Gram` library is intended to store the *full graph*, the relationship details (labels, properties) are stored in the `value` of the pattern element if it's a relationship pattern, OR the `Pattern` structure uses the `elements` list to imply connections.
    - **Critical Observation**: The current parser `parseRelationship` returns `Pattern (value left) [right]`. It *discards* the relationship kind (`-->`) and attributes!
    - **TODO**: The corpus tests might pass *parsing*, but the data loss is a significant correctness issue for a "Graph Data" library.
    - *However*, for this feature (Parsing Conformance), the primary goal is to *accept valid syntax*.
    - *Future Work*: If relationship data needs to be preserved, `Pattern` or `Subject` might need extension, or `value` needs to capture the relationship attributes.

### Subject

```haskell
data Subject = Subject
  { symbol     :: Symbol
  , labels     :: Set String
  , properties :: Map String Value
  }
```

## Validation Rules

1.  **Syntax Validity**: The parser must accept any string defined as valid by the `tree-sitter-gram` corpus.
2.  **Error Handling**: The parser must reject strings marked as `:error` in the corpus.

