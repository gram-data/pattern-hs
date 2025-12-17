# Type Signatures: Codefence Multiline Strings

**Feature**: 024-codefence-strings  
**Date**: 2025-12-17

## New Functions

### Gram.Parse Module

```haskell
-- | Parse a plain fenced string (codefence without tag).
--
-- Recognizes the syntax:
-- @
-- ```
-- content here
-- can span multiple lines
-- ```
-- @
--
-- Returns the content between the opening and closing fences as a VString.
-- The opening fence must be immediately followed by a newline.
-- Content may be empty.
--
-- === Examples
--
-- >>> parse parseFencedString "" "```\nHello World\n```"
-- Right (VString "Hello World\n")
--
-- >>> parse parseFencedString "" "```\n```"
-- Right (VString "")
--
-- === Errors
--
-- Fails if:
-- - Opening fence is not followed by newline
-- - Closing fence is missing
parseFencedString :: Parser Value

-- | Parse a tagged fenced string (codefence with tag).
--
-- Recognizes the syntax:
-- @
-- ```tag
-- content here
-- can span multiple lines
-- ```
-- @
--
-- The tag must be a valid symbol immediately following the opening fence.
-- Returns a VTaggedString with the tag and content.
--
-- === Examples
--
-- >>> parse parseTaggedFencedString "" "```md\n# Title\n```"
-- Right (VTaggedString "md" "# Title\n")
--
-- >>> parse parseTaggedFencedString "" "```json\n{\"key\": \"value\"}\n```"
-- Right (VTaggedString "json" "{\"key\": \"value\"}\n")
--
-- === Errors
--
-- Fails if:
-- - No valid symbol follows opening fence
-- - Tag is not followed by newline
-- - Closing fence is missing
parseTaggedFencedString :: Parser Value

-- | Parse the content of a fenced string.
--
-- Captures all characters between the opening fence (after newline)
-- and the closing fence. The closing fence must be three backticks
-- at the start of a line (preceded by newline or at start).
--
-- Content may contain:
-- - Newlines
-- - Single backticks
-- - Double backticks
-- - Any other characters
--
-- Content may NOT contain three consecutive backticks at line start.
parseFencedContent :: Parser String
```

### Gram.Serialize Module

```haskell
-- | Character threshold for codefence serialization.
--
-- Strings with length greater than this value will be serialized
-- using codefence format. Length is measured as total character count
-- including newline characters.
--
-- >>> codefenceThreshold
-- 120
codefenceThreshold :: Int

-- | Serialize a string value, choosing format based on length.
--
-- For strings of 120 characters or fewer:
--   - Uses double-quoted format: "content"
--   - Escapes special characters (quotes, backslashes, newlines)
--
-- For strings exceeding 120 characters:
--   - Uses codefence format: ```\ncontent\n```
--   - Content is included verbatim (no escaping needed)
--
-- === Examples
--
-- >>> serializeStringValue "Hello"
-- "\"Hello\""
--
-- >>> serializeStringValue (replicate 121 'x')
-- "```\n" ++ replicate 121 'x' ++ "\n```"
serializeStringValue :: String -> String

-- | Serialize a tagged string value, choosing format based on content length.
--
-- For content of 120 characters or fewer:
--   - Uses inline format: tag`content`
--
-- For content exceeding 120 characters:
--   - Uses tagged codefence format: ```tag\ncontent\n```
--
-- === Examples
--
-- >>> serializeTaggedStringValue "md" "# Title"
-- "md`# Title`"
--
-- >>> serializeTaggedStringValue "md" (replicate 121 'x')
-- "```md\n" ++ replicate 121 'x' ++ "\n```"
serializeTaggedStringValue :: String -> String -> String
```

## Modified Functions

### Gram.Parse Module

```haskell
-- | Parse a string value (MODIFIED).
--
-- Now recognizes codefence syntax in addition to quoted strings.
-- Priority order:
--   1. Fenced string (```...```)
--   2. Double-quoted string ("...")
--   3. Single-quoted string ('...')
--   4. Backtick string (`...`)
--
-- === Changes from previous version
--
-- - Added: try parseFencedString alternative
parseString :: Parser String

-- | Parse a tagged string value (MODIFIED).
--
-- Now recognizes tagged codefence syntax in addition to inline form.
-- Priority order:
--   1. Tagged fenced string (```tag...```)
--   2. Inline tagged string (tag`...`)
--
-- === Changes from previous version
--
-- - Added: try parseTaggedFencedString alternative
parseTaggedString :: Parser Value
```

### Gram.Serialize Module

```haskell
-- | Serialize a Value to gram notation (MODIFIED).
--
-- === Changes from previous version
--
-- - VString: Now uses codefence format for strings > 120 chars
-- - VTaggedString: Now uses tagged codefence for content > 120 chars
--
-- All other value types unchanged.
serializeValue :: Value -> String
```

## Constants

| Name | Type | Value | Description |
|------|------|-------|-------------|
| `codefenceThreshold` | `Int` | `120` | Character count above which codefence format is used |

## Type Aliases (No Changes)

The feature uses existing types without modification:

```haskell
type Parser = Parsec Void String  -- Existing
type Value = Subject.Value.Value  -- Existing
```

