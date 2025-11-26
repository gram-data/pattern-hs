# Feature Specification: Gram Parsing Conformance

**Feature Branch**: `016-gram-parsing-conformance`  
**Created**: 2025-11-25  
**Status**: Draft  
**Input**: User description: "begin Parsing Conformance Re-assessment as detailed in 2.2 of @libs/gram/TODO.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Full Syntax Compliance (Priority: P1)

As a developer using the Gram library, I need the parser to correctly handle all valid syntax defined in the standard Gram specification (as represented by the tree-sitter-gram corpus), including complex path expressions, so that I can reliably process any valid Gram data file.

**Why this priority**: Compliance with the standard syntax is fundamental for the library's utility and interoperability. Without full support (specifically for missing features like Paths), the library cannot be relied upon for general-purpose Gram processing.

**Independent Test**: Can be tested by running the parser against the entire `tree-sitter-gram` test corpus and verifying zero failures.

**Acceptance Scenarios**:

1. **Given** a valid Gram file containing Path syntax (e.g., `(n1)-[e1]->(n2)`), **When** the file is parsed, **Then** the result is a correct in-memory representation of the nodes and edges.
2. **Given** the full `tree-sitter-gram` corpus of test cases, **When** the test suite is executed, **Then** all tests pass without error.
3. **Given** a Gram file with valid but complex nested structures, **When** parsed, **Then** the structure is preserved correctly in the output model.

---

### User Story 2 - Parse Error Reporting (Priority: P2)

As a developer debugging Gram data, I need the parser to provide specific error information when it encounters invalid syntax, so that I can quickly locate and fix issues in my data files.

**Why this priority**: Good error reporting is essential for developer experience, though strictly secondary to the ability to parse valid code.

**Independent Test**: Can be tested by feeding invalid Gram strings to the parser and asserting that error results contain location and context information.

**Acceptance Scenarios**:

1. **Given** a Gram string with invalid syntax, **When** parsed, **Then** the result is an error result containing a descriptive error message.
2. **Given** a Gram string with a syntax error at a specific line/column, **When** parsed, **Then** the error message indicates the approximate location of the failure.

### Edge Cases

- **Empty Input**: System should handle empty strings gracefully (parse as empty graph or specific error).
- **Deeply Nested Structures**: System should handle recursion limits or deep nesting without stack overflow (implied by corpus tests).
- **Unicode/Special Characters**: System should correctly parse valid Unicode identifiers as allowed by the grammar.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST parse all syntax constructs defined in the `tree-sitter-gram` corpus without error.
- **FR-002**: The system MUST explicitly support the "Path" syntax (e.g., `(a)-->(b)` sequences) and map them to the appropriate graph data structure.
- **FR-003**: The system MUST correctly handle edge cases defined in the corpus, such as anonymous nodes, empty relationships, and complex nesting.
- **FR-004**: The system MUST provide a mechanism to run the `tree-sitter-gram` corpus tests as part of the standard test suite.
- **FR-005**: The system MUST report parse errors with sufficient detail to identify the nature of the syntax violation.

### Key Entities

- **Gram Parser**: The core component responsible for transforming text input into the Gram data model.
- **Tree-Sitter Corpus**: The authoritative set of test cases defining valid Gram syntax.
- **Path Pattern**: A specific syntax element representing a sequence of nodes and edges (e.g., `(a)-[r]->(b)`).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: 100% of tests in the `tree-sitter-gram` corpus pass.
- **SC-002**: Parse errors for invalid inputs are reported (parsing does not crash or hang).
- **SC-003**: Path syntax structures from the corpus are correctly instantiated in the data model (verified by structure inspection in tests).
