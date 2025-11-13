# Type Signatures: Traversable Instance for Pattern

**Feature**: 007-traversable-instance  
**Date**: 2025-01-28

## Overview

This document defines the public API type signatures for the Traversable instance for `Pattern`. The Traversable instance enables effectful traversal while preserving pattern structure.

---

## Core Module: Pattern.Core

### Traversable Instance

```haskell
-- | Traversable instance for Pattern that enables effectful traversal
-- while preserving pattern structure.
--
-- The Traversable instance satisfies the traversable laws:
--
-- * Naturality law: @t . traverse f = traverse (t . f)@ for applicative transformations
-- * Identity law: @traverse Identity = Identity@
-- * Composition law: @traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f@
--
-- Examples:
--
-- >>> let p = pattern "hello"
-- >>> traverse (Identity . map toUpper) p
-- Identity (Pattern { value = "HELLO", elements = [] })
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> let p = patternWith 10 [pattern 5, pattern (-3)]
-- >>> traverse validate p
-- Nothing
--
-- >>> let p = patternWith (Just 10) [Just 5, Just 3]
-- >>> sequenceA p
-- Just (patternWith 10 [pattern 5, pattern 3])
instance Traversable Pattern where
  traverse :: Applicative f => (a -> f b) -> Pattern a -> f (Pattern b)
  traverse f (Pattern v els) = 
    Pattern <$> f v <*> traverse (traverse f) els
  
  sequenceA :: Applicative f => Pattern (f a) -> f (Pattern a)
  sequenceA = traverse id
```

### Traversable Methods

#### traverse

```haskell
-- | Traverse a pattern with an effectful function while preserving structure.
--
-- The function is applied to all values in the pattern structure:
-- the decoration value and all values in element patterns (recursively).
-- The pattern structure (element count, nesting depth, element order)
-- remains unchanged. Effects are combined using applicative semantics.
--
-- Examples:
--
-- >>> traverse Identity (pattern "test")
-- Identity (Pattern { value = "test", elements = [] })
--
-- >>> let validate x = if x > 0 then Just x else Nothing
-- >>> traverse validate (pattern 5)
-- Just (Pattern { value = 5, elements = [] })
--
-- >>> traverse validate (pattern (-3))
-- Nothing
--
-- >>> let p = patternWith 10 [pattern 5, pattern 3]
-- >>> traverse validate p
-- Just (patternWith 10 [pattern 5, pattern 3])
--
-- >>> let p = patternWith 10 [pattern 5, pattern (-3)]
-- >>> traverse validate p
-- Nothing
traverse :: Applicative f => (a -> f b) -> Pattern a -> f (Pattern b)
```

**Type Signature**: `Applicative f => (a -> f b) -> Pattern a -> f (Pattern b)`

**Behavior**:
- Applies the effectful function to the decoration value
- Recursively applies the function to all element patterns
- Preserves pattern structure (element count, nesting depth, element order)
- Combines effects using applicative semantics

**Effect Combination**:
- **Maybe**: Short-circuits to Nothing if any value produces Nothing
- **Either**: Short-circuits to Left with first error if any value produces Left
- **[]**: Collects all results from all values
- **Identity**: Preserves structure without effects
- **IO**: Performs all IO operations and combines results
- **State**: Threads state through all values

**Traversable Laws**:
- **Naturality**: `t . traverse f = traverse (t . f)` for applicative transformations
- **Identity**: `traverse Identity = Identity`
- **Composition**: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

#### sequenceA

```haskell
-- | Sequence applicative effects from a pattern.
--
-- Converts a pattern containing applicative values into an applicative
-- value containing a pattern. Effects are combined using applicative semantics.
--
-- Example:
--
-- >>> let p = patternWith (Just 10) [Just 5, Just 3]
-- >>> sequenceA p
-- Just (patternWith 10 [pattern 5, pattern 3])
--
-- >>> let p = patternWith (Just 10) [Just 5, Nothing]
-- >>> sequenceA p
-- Nothing
--
-- >>> let p = patternWith (Right 10) [Right 5, Left "error"]
-- >>> sequenceA p
-- Left "error"
sequenceA :: Applicative f => Pattern (f a) -> f (Pattern a)
```

**Type Signature**: `Applicative f => Pattern (f a) -> f (Pattern a)`

**Behavior**:
- Converts `Pattern (f a)` to `f (Pattern a)`
- Combines effects using applicative semantics
- Preserves pattern structure
- Default implementation: `sequenceA = traverse id`

**Effect Combination**:
- **Maybe**: Returns Just pattern if all values are Just, Nothing otherwise
- **Either**: Returns Right pattern if all values are Right, Left with first error otherwise
- **[]**: Collects all combinations of values
- **Identity**: Unwraps Identity values
- **IO**: Performs all IO operations and combines results
- **State**: Threads state through all values

---

## Module Exports

The `Pattern.Core` module exports:

- `Traversable` instance for `Pattern` (via `traverse` and `sequenceA`)

The main `Pattern` module re-exports the Traversable instance.

---

## Type Safety Guarantees

1. **Type transformation**: The Traversable instance allows safe transformation from `Pattern a` to `f (Pattern b)` via any effectful function `a -> f b`
2. **Structure preservation**: Pattern structure (element count, nesting depth, element order) is guaranteed to remain unchanged
3. **Traversable laws**: The implementation satisfies traversable laws (verified by property-based tests)
4. **Recursive application**: Traversal is applied recursively to all nested patterns
5. **Effect combination**: Effects are combined using standard applicative semantics

---

## Usage Examples

### Basic Effectful Traversal

```haskell
-- Traverse with Identity (no effects)
let p = pattern "hello"
    p' = traverse Identity p
-- p' = Identity (pattern "hello")

-- Traverse with Maybe (validation)
let validate x = if x > 0 then Just x else Nothing
let p = pattern 5
    p' = traverse validate p
-- p' = Just (pattern 5)

-- Traverse with Either (error handling)
let validateEither x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
let p = pattern 5
    p' = traverse validateEither p
-- p' = Right (pattern 5)
```

### Validation with Maybe

```haskell
-- Validate all values in a pattern
let validate x = if x > 0 then Just x else Nothing
let p = patternWith 10 [pattern 5, pattern 3]
    p' = traverse validate p
-- p' = Just (patternWith 10 [pattern 5, pattern 3])

-- Validation fails if any value is invalid
let p = patternWith 10 [pattern 5, pattern (-3)]
    p' = traverse validate p
-- p' = Nothing
```

### Validation with Either

```haskell
-- Validate with error messages
let validateEither x = if x > 0 then Right x else Left ("Invalid: " ++ show x)
let p = patternWith 10 [pattern 5, pattern 3]
    p' = traverse validateEither p
-- p' = Right (patternWith 10 [pattern 5, pattern 3])

-- First error is returned
let p = patternWith 10 [pattern 5, pattern (-3)]
    p' = traverse validateEither p
-- p' = Left "Invalid: -3"
```

### Sequencing Effects

```haskell
-- Sequence Maybe values
let p = patternWith (Just 10) [Just 5, Just 3]
    p' = sequenceA p
-- p' = Just (patternWith 10 [pattern 5, pattern 3])

-- Sequence Either values
let p = patternWith (Right 10) [Right 5, Right 3]
    p' = sequenceA p
-- p' = Right (patternWith 10 [pattern 5, pattern 3])

-- Sequence fails if any value fails
let p = patternWith (Just 10) [Just 5, Nothing]
    p' = sequenceA p
-- p' = Nothing
```

### Nested Patterns

```haskell
-- Traverse nested patterns
let validate x = if x > 0 then Just x else Nothing
let p = patternWith 10 
      [ patternWith 5 [pattern 1, pattern 2]
      , patternWith 3 [pattern 1]
      ]
    p' = traverse validate p
-- p' = Just (patternWith 10 
--              [ patternWith 5 [pattern 1, pattern 2]
--              , patternWith 3 [pattern 1]
--              ])

-- Validation fails if any nested value is invalid
let p = patternWith 10 
      [ patternWith 5 [pattern 1, pattern (-2)]
      , patternWith 3 [pattern 1]
      ]
    p' = traverse validate p
-- p' = Nothing
```

### IO Operations

```haskell
-- Traverse with IO
let readValue :: String -> IO Int
    readValue s = readIO s
let p = patternWith "10" [pattern "5", pattern "3"]
    p' = traverse readValue p
-- p' :: IO (Pattern Int)
-- p' = IO (patternWith 10 [pattern 5, pattern 3])
```

### Stateful Computations

```haskell
-- Traverse with State
let addState :: Int -> State Int Int
    addState x = do
      s <- get
      put (s + x)
      return (s + x)
let p = patternWith 10 [pattern 5, pattern 3]
    p' = traverse addState p
-- p' :: State Int (Pattern Int)
-- Running with initial state 0:
-- runState p' 0 = (patternWith 0 [pattern 5, pattern 8], 18)
```

---

## Traversable Laws Verification

The Traversable instance must satisfy the following laws (verified by property-based tests):

### Naturality Law

```haskell
t . traverse f = traverse (t . f)
```

**Test**: For any applicative transformation `t` and function `f`, `t . traverse f = traverse (t . f)`

### Identity Law

```haskell
traverse Identity = Identity
```

**Test**: For any pattern `p`, `traverse Identity p == Identity p`

### Composition Law

```haskell
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
```

**Test**: For any pattern `p` and functions `f` and `g`, `traverse (Compose . fmap g . f) p == (Compose . fmap (traverse g) . traverse f) p`

---

## Future Extensions

This phase defines only the Traversable instance. Future phases may add:

- Additional traversal utilities built on Traversable
- Specialized traversal operations for specific use cases
- Performance optimizations for large patterns

