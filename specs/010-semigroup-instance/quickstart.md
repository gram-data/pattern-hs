# Quickstart: Semigroup Instance for Pattern

**Feature**: 010-semigroup-instance  
**Date**: 2025-01-27  
**Status**: Design Complete

## Overview

The `Semigroup` instance for `Pattern` enables combining patterns by concatenating their elements and combining their values using the value type's Semigroup instance. This enables incremental pattern construction using standard Haskell combinators.

## Basic Usage

### Combining Patterns

```haskell
import Pattern.Core

-- Combine atomic patterns
let p1 = pattern "a"
let p2 = pattern "b"
p1 <> p2  -- Pattern { value = "ab", elements = [] }

-- Combine patterns with elements
let p3 = patternWith "root" [pattern "a", pattern "b"]
let p4 = patternWith "root" [pattern "c", pattern "d"]
p3 <> p4  -- Pattern { value = "rootroot", elements = [pattern "a", pattern "b", pattern "c", pattern "d"] }

-- Combine patterns with different values
let p5 = patternWith "prefix" [pattern "x"]
let p6 = patternWith "suffix" [pattern "y"]
p5 <> p6  -- Pattern { value = "prefixsuffix", elements = [pattern "x", pattern "y"] }
```

### Incremental Pattern Construction

```haskell
-- Start with atomic patterns
let atoms = [pattern "a", pattern "b", pattern "c"]

-- Build pattern incrementally
let combined = foldr1 (<>) atoms
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Or use sconcat
import Data.Semigroup (sconcat)
let combined2 = sconcat (pattern "a" :| [pattern "b", pattern "c"])
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }
```

### Combining Nested Patterns

```haskell
-- Create nested patterns
let nested1 = patternWith "outer" [patternWith "inner1" [pattern "a"]]
let nested2 = patternWith "outer" [patternWith "inner2" [pattern "b"]]

-- Combine nested patterns
nested1 <> nested2
-- Result: Pattern { value = "outerouter", elements = [Pattern { value = "inner1", elements = [pattern "a"] }, Pattern { value = "inner2", elements = [pattern "b"] }] }
```

## Standard Semigroup Combinators

### sconcat

Combine a non-empty list of patterns:

```haskell
import Data.Semigroup (sconcat)

let patterns = pattern "a" :| [pattern "b", pattern "c"]
sconcat patterns
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }
```

### stimes

Repeat a pattern n times:

```haskell
import Data.Semigroup (stimes)

stimes 3 (pattern "a")
-- Result: Pattern { value = "aaa", elements = [pattern "a", pattern "a", pattern "a"] }

stimes 2 (patternWith "root" [pattern "x"])
-- Result: Pattern { value = "rootroot", elements = [pattern "x", pattern "x"] }
```

## Use Cases

### Use Case 1: Incremental Pattern Construction

Build patterns incrementally by combining smaller patterns:

```haskell
-- Build a pattern step by step
let step1 = pattern "start"
let step2 = step1 <> pattern "middle"
let step3 = step2 <> pattern "end"
-- step3: Pattern { value = "startmiddleend", elements = [pattern "start", pattern "middle", pattern "end"] }
```

### Use Case 2: Pattern Merging in Graph Operations

Combine patterns representing graph elements:

```haskell
-- Node pattern
let nodePattern = patternWith "Node" [pattern "id1"]

-- Edge pattern
let edgePattern = patternWith "Edge" [pattern "id2", pattern "id3"]

-- Combined subgraph
let subgraph = nodePattern <> edgePattern
-- Result: Pattern { value = "NodeEdge", elements = [pattern "id1", pattern "id2", pattern "id3"] }
```

### Use Case 3: Pattern Accumulation

Accumulate patterns during fold operations:

```haskell
-- Accumulate patterns from a list
let patterns = [pattern "a", pattern "b", pattern "c"]
let accumulated = foldr1 (<>) patterns
-- Result: Pattern { value = "abc", elements = [pattern "a", pattern "b", pattern "c"] }

-- Or use foldMap
import Data.Foldable (foldMap)
let accumulated2 = foldMap id patterns
-- Result: Same as above (if Pattern has Monoid instance, which requires Semigroup)
```

## Value Type Semigroup Examples

### String Values (Concatenation)

```haskell
pattern "hello" <> pattern "world"
-- Result: Pattern { value = "helloworld", elements = [] }
```

### Sum Int Values (Addition)

```haskell
import Data.Semigroup (Sum)

pattern (Sum 5) <> pattern (Sum 3)
-- Result: Pattern { value = Sum 8, elements = [] }
```

### Product Int Values (Multiplication)

```haskell
import Data.Semigroup (Product)

pattern (Product 5) <> pattern (Product 3)
-- Result: Pattern { value = Product 15, elements = [] }
```

### Custom Semigroup Instances

```haskell
-- Custom Semigroup for combining lists
newtype ListSemigroup a = ListSemigroup [a]
  deriving (Show, Eq)

instance Semigroup (ListSemigroup a) where
  ListSemigroup xs <> ListSemigroup ys = ListSemigroup (xs ++ ys)

pattern (ListSemigroup [1, 2]) <> pattern (ListSemigroup [3, 4])
-- Result: Pattern { value = ListSemigroup [1, 2, 3, 4], elements = [] }
```

## Edge Cases

### Atomic Patterns

```haskell
-- Combine two atomic patterns
pattern "a" <> pattern "b"
-- Result: Pattern { value = "ab", elements = [] }
```

### Atomic Pattern with Pattern Having Elements

```haskell
pattern "a" <> patternWith "b" [pattern "x", pattern "y"]
-- Result: Pattern { value = "ab", elements = [pattern "x", pattern "y"] }
```

### Different Element Counts

```haskell
patternWith "a" [pattern "x"] <> patternWith "b" [pattern "y", pattern "z"]
-- Result: Pattern { value = "ab", elements = [pattern "x", pattern "y", pattern "z"] }
```

### Different Nesting Depths

```haskell
patternWith "a" [pattern "x"] 
<> patternWith "b" [patternWith "c" [pattern "y", pattern "z"]]
-- Result: Pattern { value = "ab", elements = [pattern "x", Pattern { value = "c", elements = [pattern "y", pattern "z"] }] }
```

## Type Constraints

The `Semigroup` instance requires the value type to have a `Semigroup` instance:

```haskell
-- This works (String has Semigroup instance)
pattern "a" <> pattern "b" :: Pattern String

-- This doesn't work if CustomType doesn't have Semigroup instance
pattern customValue1 <> pattern customValue2 :: Pattern CustomType
-- Error: No instance for (Semigroup CustomType)
```

## Associativity

The combination operation is associative:

```haskell
let p1 = pattern "a"
let p2 = pattern "b"
let p3 = pattern "c"

(p1 <> p2) <> p3 == p1 <> (p2 <> p3)  -- True
```

This enables safe use of `sconcat` and other Semigroup combinators.

## Notes

- Element order is preserved: left pattern's elements come first, then right pattern's elements
- Value combination uses the value type's Semigroup instance
- Nested structures are preserved as elements in the combined pattern
- The instance enables incremental pattern construction using standard Haskell combinators
- Performance is O(n+m) where n and m are the number of elements in the two patterns being combined

