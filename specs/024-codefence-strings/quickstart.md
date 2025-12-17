# Quickstart: Codefence Multiline Strings

**Feature**: 024-codefence-strings  
**Date**: 2025-12-17

## Overview

This feature adds support for multiline strings using triple-backtick codefence syntax in gram notation. Both plain and tagged codefences are supported.

## Syntax

### Plain Codefence String

```gram
(:Example {description: ```
This is a multiline string.
It can span multiple lines without escaping.
All whitespace and newlines are preserved.
```})
```

Parses as: `VString "This is a multiline string.\nIt can span multiple lines without escaping.\nAll whitespace and newlines are preserved.\n"`

### Tagged Codefence String

```gram
(:Example {prompt: ```md
# Markdown Content

This is **bold** and *italic* text.

- List item 1
- List item 2
```})
```

Parses as: `VTaggedString "md" "# Markdown Content\n\nThis is **bold** and *italic* text.\n\n- List item 1\n- List item 2\n"`

## Usage Examples

### Parsing Codefence Strings

```haskell
import Gram.Parse (fromGram)
import Subject.Value (Value(..))

-- Parse a node with a plain codefence property
example1 :: IO ()
example1 = do
  let input = "(:Doc {content: ```\nLine 1\nLine 2\n```})"
  case fromGram input of
    Right pattern -> print pattern  -- Shows VString with newlines
    Left err -> print err

-- Parse a node with a tagged codefence property
example2 :: IO ()
example2 = do
  let input = "(:Doc {code: ```haskell\nmain = putStrLn \"Hello\"\n```})"
  case fromGram input of
    Right pattern -> print pattern  -- Shows VTaggedString "haskell" "..."
    Left err -> print err
```

### Serializing with Automatic Codefence

```haskell
import Gram.Serialize (toGram)
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Short strings use quoted format
shortExample :: String
shortExample = toGram pattern
  where
    props = Map.fromList [("msg", VString "Hello")]
    pattern = Pattern (Subject (Symbol "n") Set.empty props) []
-- Result: (n {msg:"Hello"})

-- Long strings (>120 chars) automatically use codefence
longExample :: String
longExample = toGram pattern
  where
    longContent = replicate 150 'x'
    props = Map.fromList [("data", VString longContent)]
    pattern = Pattern (Subject (Symbol "n") Set.empty props) []
-- Result: (n {data:```
-- xxxxxx...xxx
-- ```})

-- Tagged strings follow same rules
taggedExample :: String
taggedExample = toGram pattern
  where
    shortMd = "# Title"
    longMd = "# Title\n\n" ++ replicate 150 'x'
    props = Map.fromList 
      [ ("short", VTaggedString "md" shortMd)   -- Uses: md`# Title`
      , ("long", VTaggedString "md" longMd)     -- Uses: ```md\n...\n```
      ]
    pattern = Pattern (Subject (Symbol "n") Set.empty props) []
```

### Round-Trip Example

```haskell
import Gram.Parse (fromGram)
import Gram.Serialize (toGram)

-- Parse, serialize, parse again - content is preserved
roundTrip :: String -> Either String Bool
roundTrip input = do
  pattern1 <- either (Left . show) Right $ fromGram input
  let serialized = toGram pattern1
  pattern2 <- either (Left . show) Right $ fromGram serialized
  return (pattern1 == pattern2)

-- Test with codefence
test :: IO ()
test = do
  let input = "(:Example {text: ```\nMultiline\nContent\n```})"
  case roundTrip input of
    Right True -> putStrLn "Round-trip successful!"
    Right False -> putStrLn "Content mismatch after round-trip"
    Left err -> putStrLn $ "Error: " ++ err
```

## Serialization Threshold

The serializer automatically chooses between quoted and codefence format based on string length:

| Length | Plain String | Tagged String |
|--------|--------------|---------------|
| ≤ 120 chars | `"escaped content"` | `` tag`content` `` |
| > 120 chars | ```` ```\ncontent\n``` ```` | ```` ```tag\ncontent\n``` ```` |

**Note**: Length is measured as total character count including newlines.

## Common Patterns

### Embedding Code Snippets

```gram
(:CodeExample {
  language: "python",
  code: ```python
def hello():
    print("Hello, World!")

if __name__ == "__main__":
    hello()
```
})
```

### Embedding Documentation

```gram
(:Documentation {
  readme: ```md
# Project Name

A brief description of the project.

## Installation

```bash
npm install my-project
```

## Usage

Import and use the module...
```
})
```

### Embedding Structured Data

```gram
(:Config {
  schema: ```json
{
  "type": "object",
  "properties": {
    "name": {"type": "string"},
    "age": {"type": "integer"}
  },
  "required": ["name"]
}
```
})
```

## Edge Cases

### Empty Codefence

```gram
// Valid - produces empty string
(:Example {empty: ```
```})
```

### Content with Backticks

```gram
// Single and double backticks are allowed in content
(:Example {code: ```
const msg = `Hello ${name}`;
const nested = `outer ${`inner`}`;
```})
```

### Boundary Case (Exactly 120 Characters)

A string with exactly 120 characters will use quoted format (threshold is strictly greater than 120).

