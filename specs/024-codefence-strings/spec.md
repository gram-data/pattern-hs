# Feature Specification: Codefence Multiline Strings

**Feature Branch**: `024-codefence-strings`  
**Created**: 2025-12-17  
**Status**: Draft  
**Input**: User description: "Support multiline strings using codefences of triple-backtick. Both plain and tagged strings should be supported."

## Clarifications

### Session 2025-12-17

- Q: When should the serializer output codefence format vs regular string format? → A: Strings longer than 120 characters should be output using codefences (both plain and tagged strings)
- Q: Should short multiline strings (under 120 chars with newlines) use codefence format? → A: No, length-only rule applies; escape newlines in short strings
- Q: How is the 120-character threshold measured? → A: Total character count including newlines

## User Scenarios & Testing

### User Story 1 - Plain Codefence String (Priority: P1)

As a developer authoring gram notation, I want to embed multiline text content using triple-backtick codefences so that I can include formatted text, code snippets, or documentation without escaping newlines.

**Why this priority**: Plain codefence strings are the most common use case for multiline content and provide immediate value for embedding text blocks.

**Independent Test**: Can be fully tested by parsing a gram file with a plain codefence string and verifying the multiline content is preserved correctly as a `VString` value.

**Acceptance Scenarios**:

1. **Given** a gram file containing a node with a property value using triple-backtick codefence without a tag, **When** the file is parsed, **Then** the content between the opening and closing triple-backticks is captured as a `VString` value with newlines preserved.

2. **Given** a property value with an empty codefence string (```\n```), **When** parsed, **Then** an empty string value is returned.

3. **Given** a codefence string containing backticks (but not three consecutive backticks), **When** parsed, **Then** the backticks are included in the string content.

---

### User Story 2 - Tagged Codefence String (Priority: P1)

As a developer authoring gram notation, I want to embed multiline text with a format tag (like `md`, `html`, `json`) using tagged codefence syntax so that downstream systems can interpret the content appropriately.

**Why this priority**: Tagged codefence strings are equally important as plain codefences, enabling semantic markup of content type for rich text like Markdown, HTML, or structured formats like JSON.

**Independent Test**: Can be fully tested by parsing a gram file with a tagged codefence string and verifying both the tag and content are captured as a `VTaggedString` value.

**Acceptance Scenarios**:

1. **Given** a gram file containing a property value using ````md\ncontent\n```` syntax, **When** the file is parsed, **Then** the result is a `VTaggedString` with tag "md" and the multiline content preserved.

2. **Given** a tagged codefence with various common tags (html, json, cypher, sql), **When** parsed, **Then** the tag is captured exactly as written and content preserves all whitespace and newlines.

3. **Given** a tagged codefence with empty content (````md\n````), **When** parsed, **Then** a `VTaggedString` with the tag and empty string content is returned.

---

### User Story 3 - Integration with Property Records (Priority: P2)

As a developer, I want codefence strings to work seamlessly within property records so that I can use them anywhere a string value is expected.

**Why this priority**: This ensures codefence strings integrate naturally with existing gram notation patterns and don't require special handling.

**Independent Test**: Can be fully tested by parsing a node with a property record containing a codefence value alongside other property types.

**Acceptance Scenarios**:

1. **Given** a node pattern with a property record containing mixed value types including a codefence string, **When** parsed, **Then** all property values are correctly typed and the codefence content is preserved.

2. **Given** a property record with multiple codefence properties, **When** parsed, **Then** each codefence value is parsed independently and correctly.

---

### User Story 4 - Automatic Codefence Serialization (Priority: P2)

As a developer serializing gram patterns, I want long strings to automatically use codefence format so that the output is readable and matches common code formatting conventions.

**Why this priority**: Serialization completes the round-trip capability and ensures generated gram files are human-readable.

**Independent Test**: Can be fully tested by serializing a pattern with strings of various lengths and verifying the output format matches the 120-character threshold rule.

**Acceptance Scenarios**:

1. **Given** a pattern with a `VString` value of 121+ characters, **When** serialized, **Then** the output uses plain codefence format with triple-backticks.

2. **Given** a pattern with a `VString` value of 120 or fewer characters, **When** serialized, **Then** the output uses standard quote-delimited format.

3. **Given** a pattern with a `VTaggedString` value of 121+ characters, **When** serialized, **Then** the output uses tagged codefence format (````tag\ncontent\n````).

4. **Given** a pattern with a `VTaggedString` value of 120 or fewer characters, **When** serialized, **Then** the output uses standard backtick-delimited tagged format (`tag`content``).

---

### Edge Cases

- What happens when a codefence contains exactly two consecutive backticks? (Should be included in content)
- How does the parser handle a codefence that is not closed? (Should fail with clear error)
- What happens when the opening triple-backtick is not followed by a newline? (Should fail - grammar requires newline)
- How does the parser handle Windows-style line endings (CRLF)? (Should normalize to LF or preserve as-is)
- What happens when serializing a string of exactly 120 characters? (Should use standard quote format, not codefence)
- How does the serializer handle strings containing newlines that are under 120 characters? (Uses standard format with escaped newlines - length is the only criterion for codefence output)

## Requirements

### Functional Requirements

- **FR-001**: Parser MUST recognize plain codefence strings delimited by triple-backticks with a newline after the opening delimiter
- **FR-002**: Parser MUST recognize tagged codefence strings where a symbol immediately follows the opening triple-backticks before the newline
- **FR-003**: Parser MUST capture all content between the opening (after newline) and closing triple-backticks, preserving internal whitespace and newlines
- **FR-004**: Plain codefence strings MUST be parsed as `VString` values
- **FR-005**: Tagged codefence strings MUST be parsed as `VTaggedString` values with the tag and content separated
- **FR-006**: Parser MUST handle content containing single or double backticks without terminating the codefence
- **FR-007**: Parser MUST fail gracefully with a clear error message when a codefence is not properly closed
- **FR-008**: Codefence strings MUST be usable anywhere a string value is valid in gram notation (property values, annotation values)
- **FR-009**: Serializer MUST output plain strings (`VString`) longer than 120 total characters (including newlines) using codefence format
- **FR-010**: Serializer MUST output tagged strings (`VTaggedString`) longer than 120 total characters (including newlines) using tagged codefence format
- **FR-011**: Serializer MUST output strings of 120 or fewer total characters using standard quote-delimited format (preserving current behavior)
- **FR-012**: Serializer MUST escape newlines (`\n`) in strings under 120 characters that contain line breaks (length-only rule determines format)

### Key Entities

- **VString**: Existing value type representing a plain string, extended to support multiline codefence content
- **VTaggedString**: Existing value type representing a tagged string (tag + content), extended to support codefence syntax
- **Parser Combinators**: New parsing functions `parseFencedString` and `parseTaggedFencedString` to handle the codefence syntax

## Success Criteria

### Measurable Outcomes

- **SC-001**: All existing gram files with standard string types continue to parse correctly (100% backward compatibility)
- **SC-002**: The example file `examples/markdown.gram` parses successfully with correct extraction of the tagged codefence content
- **SC-003**: Users can round-trip (parse → serialize → parse) gram files containing codefence strings without data loss
- **SC-004**: Parser provides clear, actionable error messages when codefence syntax is malformed (unclosed, missing newline after opening)
- **SC-005**: Test suite includes coverage for plain codefences, tagged codefences, edge cases, and integration with property records

## Assumptions

- The tree-sitter-gram grammar (version 0.2.x) already defines codefence string syntax, so the Haskell parser is aligning with that specification
- Codefence content does not require escape sequence processing (content is captured verbatim)
- The closing triple-backtick must appear at the start of a line (following tree-sitter-gram behavior)
- Tags are symbols following the same rules as other gram symbols (alphanumeric starting with letter/underscore)
