# Research: Codefence Multiline Strings

**Feature**: 024-codefence-strings  
**Date**: 2025-12-17

## Research Questions

### 1. How does tree-sitter-gram define codefence syntax?

**Decision**: Align with existing tree-sitter-gram grammar definition.

**Rationale**: The tree-sitter-gram grammar (in `libs/gram/test-data/tree-sitter-gram/grammar.js`) already defines the codefence syntax. The Haskell parser should follow this specification exactly to ensure compatibility across all implementations.

**Key Grammar Rules**:
```javascript
// Plain fenced string
_fenced_string: ($) => seq("```", $._fenced_string_body),
_fenced_string_body: ($) => seq(
  "\n",
  field("content", optional(alias($._fenced_string_content, $.string_content))),
  "```",
),
_fenced_string_content: ($) => token.immediate(/([^`]|`[^`]|``[^`])+/),

// Tagged fenced string (second alternative in tagged_string)
tagged_string: ($) => choice(
  // ... inline backtick form ...
  seq(
    "```",
    field("tag", $.symbol),
    "\n",
    field("content", optional(alias($._fenced_string_content, $.string_content))),
    "```",
  ),
),
```

**Alternatives considered**:
- Custom syntax: Rejected - would break compatibility with tree-sitter-gram tooling
- Different delimiter count (e.g., 4 backticks): Rejected - not in grammar

### 2. How should Megaparsec handle multiline content capture?

**Decision**: Use `manyTill` with explicit newline handling and custom terminator detection.

**Rationale**: Megaparsec's `manyTill` combinator is ideal for capturing content until a closing delimiter. The challenge is detecting three consecutive backticks at line start without consuming them prematurely.

**Implementation Pattern**:
```haskell
parseFencedContent :: Parser String
parseFencedContent = do
  -- Capture characters until we see "\n```" (closing fence)
  content <- manyTill anyChar (try closingFence)
  return content
  where
    closingFence = string "\n```" <|> (eof >> return "\n```")
```

**Alternatives considered**:
- Regex-based parsing: Rejected - Megaparsec doesn't use regex natively; would add complexity
- Character-by-character state machine: Rejected - `manyTill` is cleaner and well-tested

### 3. How should the serializer decide between quote and codefence format?

**Decision**: Length-based threshold of 120 total characters (including newlines).

**Rationale**: Per spec clarifications, the serializer uses a simple length check. This provides predictable, deterministic output without content inspection beyond length.

**Implementation Pattern**:
```haskell
serializeValue :: Value -> String
serializeValue (VString s)
  | length s > 120 = "```\n" ++ s ++ "\n```"
  | otherwise = "\"" ++ escapeString s ++ "\""
serializeValue (VTaggedString tag content)
  | length content > 120 = "```" ++ tag ++ "\n" ++ content ++ "\n```"
  | otherwise = tag ++ "`" ++ content ++ "`"
```

**Alternatives considered**:
- Content-based (presence of newlines): Rejected per spec clarification - length only
- Configurable threshold: Rejected - adds complexity without clear benefit
- Always use codefence for multiline: Rejected per spec clarification

### 4. How should edge cases be handled?

**Decisions**:

| Edge Case | Handling |
|-----------|----------|
| Empty codefence (```` ```\n``` ````) | Returns empty string (`VString ""`) |
| Single/double backticks in content | Included verbatim (grammar allows) |
| Unclosed codefence | Parser error with position info |
| Missing newline after opening | Parser error (grammar requires newline) |
| CRLF line endings | Preserve as-is (no normalization) |
| Exactly 120 characters | Standard quote format (threshold is >) |
| Content ending without final newline | Add newline before closing fence |

**Rationale**: These align with tree-sitter-gram behavior and spec requirements.

## Dependencies

| Dependency | Purpose | Already Present |
|------------|---------|-----------------|
| Megaparsec | Parser combinators | ✅ Yes |
| Hspec | Unit testing | ✅ Yes |
| QuickCheck | Property tests | ✅ Yes |
| Subject.Value | VString, VTaggedString types | ✅ Yes |

No new dependencies required.

## Test Strategy

### Parser Tests

1. **Plain codefence - basic**: Parse ``` ```\nHello World\n``` ```, verify `VString "Hello World\n"`
2. **Plain codefence - empty**: Parse ``` ```\n``` ```, verify `VString ""`
3. **Plain codefence - with backticks**: Content containing `` ` `` and ``` `` ```
4. **Tagged codefence - basic**: Parse ``` ```md\n# Title\n``` ```, verify `VTaggedString "md" "# Title\n"`
5. **Tagged codefence - various tags**: html, json, cypher, sql
6. **Error cases**: Unclosed fence, missing newline after opening

### Serializer Tests

1. **Short string (≤120)**: Standard quote output
2. **Long string (>120)**: Codefence output
3. **Exactly 120 chars**: Standard quote (threshold is strictly >)
4. **Short string with newlines**: Escaped newlines in quotes
5. **Long tagged string**: Tagged codefence format
6. **Short tagged string**: Backtick format

### Round-Trip Tests (Property-based)

```haskell
prop_roundTrip :: String -> Bool
prop_roundTrip s = 
  case fromGram (toGram (makePattern s)) of
    Right p -> extractString p == s
    Left _ -> False
```

## Resolved Clarifications

| Item | Resolution |
|------|------------|
| Serialization threshold | 120 characters total (including newlines) |
| Multiline short strings | Use escaped quotes, not codefence |
| Character counting | Total count including newline characters |

