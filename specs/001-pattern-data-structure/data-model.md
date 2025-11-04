# Data Model: Pattern Data Structure

**Feature**: 001-pattern-data-structure  
**Date**: 2025-01-27

## Core Entity: Pattern

### Definition

The `Pattern` type is a recursive tree structure that serves as a generalized representation of graph elements.

### Structure

```haskell
data Pattern v = Pattern 
  { value    :: v
  , elements :: [Pattern v]
  } deriving (Functor, Foldable, Traversable)
```

### Fields

- **value** (`v`): The value associated with this pattern node. Type parameter `v` allows for different value types.
- **elements** (`[Pattern v]`): A list of child patterns, forming the recursive tree structure.

### Type Constraints

- `Pattern` is parameterized over value type `v`
- `Pattern` must be an instance of `Functor`, `Foldable`, and `Traversable` (standard typeclass instances)
- These instances enable structural transformations while preserving the tree shape

### Relationships

- **Self-referential**: Each `Pattern` contains zero or more child `Pattern` values
- **Graph interpretation**: A `Pattern` can be interpreted as different graph elements (node, relationship, subgraph, path) based on its structure

---

## Graph Element Classifications

### Node

A `Pattern` is interpreted as a **node** when:
- It has no child elements that are graph elements themselves
- Typically: `elements p == []` or all elements are leaf nodes

**Validation**: `isNode :: Pattern v -> Bool`

### Relationship

A `Pattern` is interpreted as a **relationship** when:
- It has exactly 2 child elements
- Both child elements are nodes (leaf patterns)

**Validation**: `isRelationship :: Pattern v -> Bool`

**Constraints**:
- `length (elements p) == 2`
- `all isNode (elements p)`

### Subgraph

A `Pattern` is interpreted as a **subgraph** when:
- All child elements are graph elements (nodes, relationships, or other subgraphs)

**Validation**: `isSubgraph :: Pattern v -> Bool`

**Constraints**:
- `all isGraphElement (elements p)`

### Path

A `Pattern` is interpreted as a **path** when:
- It is a subgraph
- All relationships in the path chain correctly (target of one equals source of next)

**Validation**: `isPath :: Pattern v -> Bool`

**Constraints**:
- `isSubgraph p`
- `chainsCorrectly (elements p)`

---

## Graph Views

### GraphView Typeclass

A `GraphView` defines how to interpret a `Pattern` as a graph element in a specific graph category.

```haskell
class GraphView view where
  type Direction view :: *
  
  interpretNode :: view -> Pattern v -> Bool
  interpretRel  :: view -> Pattern v -> Bool
  
  direction :: view -> Pattern v -> Direction view
  canChain  :: view -> Pattern v -> Pattern v -> Bool
  
  toGraph :: view -> Pattern v -> Graph (Direction view) v
```

### Standard Views

#### DirectedView

- **Direction type**: `Ordered` (source → target)
- **Interpretation**: Relationships have explicit direction
- **Chaining**: `target r1 == source r2`

#### UndirectedView

- **Direction type**: `Unordered` (edge set)
- **Interpretation**: Relationships are undirected
- **Chaining**: Relationships share common nodes

---

## Pattern Morphisms

### Type

```haskell
type PatternMorphism v w = Pattern v -> Pattern w
```

### Standard Morphisms

#### homomorphism

Structure-preserving map that transforms values:

```haskell
homomorphism :: (v -> w) -> PatternMorphism v w
homomorphism f = fmap f
```

#### forget

Forgetful morphism that removes value information:

```haskell
forget :: PatternMorphism v ()
forget = forgetValues
forgetValues :: Pattern v -> Pattern ()
forgetValues = fmap (const ())
```

---

## Graph Representation

### Graph Type

```haskell
data Graph dir v = Graph
  { nodes :: Set (Pattern v)
  , edges :: Set (Edge dir v)
  }
```

### Edge Types

```haskell
data Edge dir v where
  DirectedEdge   :: Pattern v -> Pattern v -> Edge Ordered v
  UndirectedEdge :: Set (Pattern v) -> Edge Unordered v
```

---

## Validation Rules

### Pattern Structure

1. **Well-formed**: All `Pattern` values must be finite (no infinite recursion)
2. **Type consistency**: All elements in a `Pattern` must have the same value type `v`

### Graph Element Classification

1. **Mutual exclusivity**: A pattern can be classified as node, relationship, subgraph, or path, but not multiple simultaneously
2. **Hierarchical**: Classification depends on child element structure
3. **Recursive**: Classification applies recursively to child elements

### Category-Theoretic Laws

1. **Functor laws**: `fmap` must preserve identity and composition
2. **Naturality**: Graph view transformations must be natural transformations
3. **Composition**: Pattern morphisms must compose correctly

---

## State Transitions

### Pattern Construction

- **Empty pattern**: `Pattern v []` - represents a node with value `v`
- **Node creation**: `Pattern v []`
- **Relationship creation**: `Pattern v [node1, node2]` where `node1` and `node2` are leaf patterns
- **Subgraph creation**: `Pattern v [elem1, elem2, ...]` where all elements are graph elements

### Pattern Transformation

- **Value transformation**: `fmap f pattern` - applies function to all values
- **Structure transformation**: Pattern morphisms that preserve or modify structure
- **View transformation**: Applying different `GraphView` instances to the same pattern

---

## Design Principles

1. **Schema-lazy**: Patterns don't commit to specific graph semantics; interpretation happens in the view
2. **Compositional**: Views can be composed, stacked, or swapped without changing underlying patterns
3. **Open-ended**: New views can be defined for any graph-like interpretation
4. **Categorical**: Each view defines a functor; forgetful pattern matching uses functor composition

