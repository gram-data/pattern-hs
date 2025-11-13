# Quickstart: Traversable Instance for Pattern

**Feature**: 007-traversable-instance  
**Date**: 2025-01-28

## Overview

This quickstart guide demonstrates how to use the Traversable instance for `Pattern` to perform effectful operations while preserving pattern structure. The Traversable instance enables you to validate values, handle errors, perform IO operations, and work with stateful computations over patterns.

---

## Installation

The Traversable instance is part of the `pattern` package. Once the project is built:

```bash
# Using Cabal
cabal install pattern

# Or add to your project's dependencies
# In your .cabal file:
build-depends: pattern >= 0.1.0.0
```

---

## Basic Usage

### Importing

```haskell
import Pattern
-- Traversable instance is automatically available via the Pattern module
import Control.Applicative (Identity)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
```

### Traversing with Identity

```haskell
-- Traverse with Identity (no effects, preserves structure)
let p = pattern "hello"
    p' = traverse Identity p
-- p' = Identity (pattern "hello")

-- Unwrap Identity to get the pattern
let Identity result = traverse Identity p
-- result = pattern "hello"
```

### Validating with Maybe

```haskell
-- Define a validation function
let validate x = if x > 0 then Just x else Nothing

-- Validate an atomic pattern
let p = pattern 5
    p' = traverse validate p
-- p' = Just (pattern 5)

-- Validation fails for invalid values
let p = pattern (-3)
    p' = traverse validate p
-- p' = Nothing

-- Validate patterns with multiple elements
let p = patternWith 10 [pattern 5, pattern 3]
    p' = traverse validate p
-- p' = Just (patternWith 10 [pattern 5, pattern 3])

-- Validation fails if any value is invalid
let p = patternWith 10 [pattern 5, pattern (-3)]
    p' = traverse validate p
-- p' = Nothing
```

### Validating with Either

```haskell
-- Define a validation function with error messages
let validateEither x = 
      if x > 0 
      then Right x 
      else Left ("Invalid value: " ++ show x)

-- Validate with error handling
let p = pattern 5
    p' = traverse validateEither p
-- p' = Right (pattern 5)

-- Get error message for invalid values
let p = pattern (-3)
    p' = traverse validateEither p
-- p' = Left "Invalid value: -3"

-- First error is returned
let p = patternWith 10 [pattern 5, pattern (-3), pattern (-5)]
    p' = traverse validateEither p
-- p' = Left "Invalid value: -3"
```

### Sequencing Effects

```haskell
-- Sequence Maybe values
let p = patternWith (Just 10) [Just 5, Just 3]
    p' = sequenceA p
-- p' = Just (patternWith 10 [pattern 5, pattern 3])

-- Sequence fails if any value is Nothing
let p = patternWith (Just 10) [Just 5, Nothing]
    p' = sequenceA p
-- p' = Nothing

-- Sequence Either values
let p = patternWith (Right 10) [Right 5, Right 3]
    p' = sequenceA p
-- p' = Right (patternWith 10 [pattern 5, pattern 3])

-- Sequence fails with first error
let p = patternWith (Right 10) [Right 5, Left "error"]
    p' = sequenceA p
-- p' = Left "error"
```

---

## Common Patterns

### Validation Workflow

```haskell
-- Step 1: Define validation function
let validatePositive x = if x > 0 then Just x else Nothing

-- Step 2: Create pattern with values to validate
let p = patternWith 10 [pattern 5, pattern 3, pattern 7]

-- Step 3: Traverse with validation
case traverse validatePositive p of
  Just validated -> 
    -- All values are valid, use validated pattern
    print "All values valid"
  Nothing -> 
    -- Some value failed validation
    print "Validation failed"
```

### Error Collection

```haskell
-- Collect all validation errors (using [] instead of Either for multiple errors)
let validateList x = if x > 0 then [x] else ["Invalid: " ++ show x]

-- This collects all results (including errors)
let p = patternWith 10 [pattern 5, pattern (-3)]
    p' = traverse validateList p
-- p' = [patternWith 10 [pattern 5, pattern (-3)]]
-- Note: [] applicative collects all combinations, not errors
-- For error collection, use Either with custom error types
```

### Nested Pattern Validation

```haskell
-- Validate deeply nested patterns
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
-- Traverse with IO operations
let readValue :: String -> IO Int
    readValue s = readIO s

let p = patternWith "10" [pattern "5", pattern "3"]
    p' = traverse readValue p
-- p' :: IO (Pattern Int)

-- Execute IO and get result
result <- traverse readValue p
-- result = patternWith 10 [pattern 5, pattern 3]
```

### Stateful Computations

```haskell
import Control.Monad.State

-- Traverse with State
let addState :: Int -> State Int Int
    addState x = do
      s <- get
      put (s + x)
      return (s + x)

let p = patternWith 10 [pattern 5, pattern 3]
    p' = traverse addState p
-- p' :: State Int (Pattern Int)

-- Run stateful computation
let (result, finalState) = runState p' 0
-- result = patternWith 0 [pattern 5, pattern 8]
-- finalState = 18
```

---

## Advanced Examples

### Custom Validation Types

```haskell
-- Define custom validation result type
data ValidationResult = Valid Int | Invalid String
  deriving (Show, Eq)

instance Applicative (Either String) where
  -- Either is already an Applicative

-- Custom validation function
let validateCustom x = 
      if x > 0 && x < 100
      then Right x
      else Left ("Value out of range: " ++ show x)

let p = patternWith 50 [pattern 25, pattern 75]
    p' = traverse validateCustom p
-- p' = Right (patternWith 50 [pattern 25, pattern 75])
```

### Combining Multiple Validations

```haskell
-- Chain multiple validations
let validatePositive x = if x > 0 then Just x else Nothing
let validateEven x = if even x then Just x else Nothing

-- First validate positive, then validate even
let p = pattern 10
    p' = traverse validatePositive p >>= traverse validateEven
-- p' = Just (pattern 10)

-- If first validation fails, second is not applied
let p = pattern (-5)
    p' = traverse validatePositive p >>= traverse validateEven
-- p' = Nothing
```

### Working with Optional Values

```haskell
-- Traverse patterns containing Maybe values
let p = patternWith (Just 10) [Just 5, Just 3]
    p' = sequenceA p
-- p' = Just (patternWith 10 [pattern 5, pattern 3])

-- Handle Nothing values
let p = patternWith (Just 10) [Just 5, Nothing, Just 3]
    p' = sequenceA p
-- p' = Nothing
```

---

## Best Practices

1. **Use Maybe for simple validation**: When you only need to know if validation succeeded or failed
2. **Use Either for error messages**: When you need to know why validation failed
3. **Use Identity for structure preservation**: When you need to preserve structure without effects
4. **Use sequenceA for effect collection**: When you have patterns containing applicative values
5. **Test edge cases**: Always test with atomic patterns, nested patterns, and effect failures

---

## Troubleshooting

### Common Issues

1. **Type errors with applicative functors**: Ensure your applicative functor instance is properly defined
2. **Unexpected Nothing/Left results**: Check that all values in the pattern satisfy your validation function
3. **Structure not preserved**: Traversable always preserves structure; if structure changes, check your validation function

### Debugging Tips

```haskell
-- Print intermediate results
let p = patternWith 10 [pattern 5, pattern (-3)]
    result = traverse validate p
print result  -- Nothing

-- Check individual values
let validate x = 
      do
        print ("Validating: " ++ show x)
        if x > 0 then Just x else Nothing
let p = patternWith 10 [pattern 5, pattern (-3)]
    result = traverse validate p
-- Prints validation messages for each value
```

---

## Next Steps

- Read the full documentation in `Pattern.Core` module
- Explore property-based tests for traversable laws
- Check examples in `examples/examples.md`
- Review the data model in `specs/007-traversable-instance/data-model.md`

