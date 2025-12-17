# Data Model: Codefence Multiline Strings

**Feature**: 024-codefence-strings  
**Date**: 2025-12-17

## Overview

This feature extends the gram notation parsing and serialization without modifying existing data types. The `Value` type already supports the necessary string representations.

## Existing Types (Unchanged)

### Value (Subject.Value)

```haskell
-- | Value type - NO CHANGES REQUIRED
data Value
  = VInteger Integer
  | VDecimal Double
  | VBoolean Bool
  | VString String           -- ← Used for plain codefence content
  | VSymbol String
  | VTaggedString String String  -- ← Used for tagged codefence (tag, content)
  | VArray [Value]
  | VMap (Map String Value)
  | VRange RangeValue
  | VMeasurement String Double
  deriving (Eq, Ord, Show, Generic, Hashable)
```

**Key Insight**: Codefence strings are a syntactic representation, not a semantic distinction. A multiline string parsed from codefence notation is identical to one from quoted notation - both become `VString` or `VTaggedString`.

## Parsing Model

### Input Syntax Variants

| Syntax | Value Type | Example |
|--------|------------|---------|
| `"text"` | `VString "text"` | `"Hello"` |
| `'text'` | `VString "text"` | `'Hello'` |
| `` `text` `` | `VString "text"` | `` `Hello` `` |
| ```` ```\ntext\n``` ```` | `VString "text\n"` | Multiline plain |
| `` tag`text` `` | `VTaggedString "tag" "text"` | `md`# Title`` |
| ```` ```tag\ntext\n``` ```` | `VTaggedString "tag" "text\n"` | Tagged multiline |

### Parser Additions

```haskell
-- | Parse a plain fenced string (triple-backtick multiline)
-- Grammar: "```" "\n" content? "```"
parseFencedString :: Parser Value

-- | Parse a tagged fenced string 
-- Grammar: "```" symbol "\n" content? "```"
parseTaggedFencedString :: Parser Value

-- | Parse fenced content (everything until closing fence)
parseFencedContent :: Parser String
```

### Integration Point

```haskell
-- BEFORE (current parseString)
parseString :: Parser String
parseString = parseDoubleQuotedString <|> parseSingleQuotedString

-- AFTER (extended)
parseString :: Parser String
parseString = 
  try parseFencedString <|>    -- NEW: plain codefence
  parseDoubleQuotedString <|> 
  parseSingleQuotedString

-- BEFORE (current parseTaggedString) 
parseTaggedString :: Parser Value
parseTaggedString = do
  tag <- parseSymbol
  void $ char '`'
  content <- manyTill (satisfy (const True)) (char '`')
  return $ V.VTaggedString (quoteSymbol tag) content

-- AFTER (extended)
parseTaggedString :: Parser Value
parseTaggedString = 
  try parseTaggedFencedString <|>  -- NEW: tagged codefence
  parseInlineTaggedString           -- Existing inline form
```

## Serialization Model

### Output Decision Logic

```
serializeValue(VString s):
  IF length(s) > 120 THEN
    output: "```\n" + s + "\n```"
  ELSE
    output: "\"" + escape(s) + "\""

serializeValue(VTaggedString tag content):
  IF length(content) > 120 THEN
    output: "```" + tag + "\n" + content + "\n```"
  ELSE
    output: tag + "`" + content + "`"
```

### Threshold Constant

```haskell
-- | Character threshold for codefence serialization
-- Strings exceeding this length use codefence format
codefenceThreshold :: Int
codefenceThreshold = 120
```

## State Transitions

N/A - No stateful data. Parsing and serialization are pure transformations.

## Validation Rules

| Rule | Enforcement |
|------|-------------|
| Opening fence must be followed by newline | Parser fails with position error |
| Closing fence must be exactly three backticks | Parser matches `\n``` ` |
| Tag must be valid symbol | Uses existing `parseSymbol` |
| Content may be empty | `optional` in parser |

## Invariants

1. **Parsing Equivalence**: `VString` from codefence equals `VString` from quotes if content matches
2. **Round-Trip Preservation**: `fromGram(toGram(pattern)) == pattern` for all valid patterns
3. **Threshold Determinism**: Given identical content, serialization always produces identical output
4. **Backward Compatibility**: Existing gram files parse identically (no behavior change)

