-- | Unit tests for Pattern.Core module.
module Spec.Pattern.CoreSpec where

import Test.Hspec
import Pattern.Core (Pattern(..))

-- Custom type for testing
data Person = Person { personName :: String, personAge :: Int }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "Pattern.Core" $ do
    
    describe "Leaf Patterns (User Story 1)" $ do
      
      describe "Creating leaf patterns with different value types" $ do
        
        it "creates a leaf pattern with string value" $ do
          let leaf = Pattern { value = "node1", elements = [] }
          value leaf `shouldBe` "node1"
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "creates a leaf pattern with integer value" $ do
          let leaf = Pattern { value = 42, elements = [] }
          value leaf `shouldBe` (42 :: Int)
          elements leaf `shouldBe` ([] :: [Pattern Int])
        
        it "creates a leaf pattern with custom type value" $ do
          let person = Person "Alice" 30
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
          elements leaf `shouldBe` ([] :: [Pattern Person])
      
      describe "Value field accessor" $ do
        
        it "returns the correct value for a leaf pattern with string" $ do
          let leaf = Pattern { value = "test", elements = [] }
          value leaf `shouldBe` "test"
        
        it "returns the correct value for a leaf pattern with integer" $ do
          let leaf = Pattern { value = 100, elements = [] }
          value leaf `shouldBe` (100 :: Int)
        
        it "returns the correct value for a leaf pattern with custom type" $ do
          let person = Person "Bob" 25
          let leaf = Pattern { value = person, elements = [] }
          value leaf `shouldBe` person
      
      describe "Elements field accessor" $ do
        
        it "returns empty list for leaf pattern" $ do
          let leaf = Pattern { value = "leaf", elements = [] }
          elements leaf `shouldBe` ([] :: [Pattern String])
        
        it "returns empty list for leaf pattern with different value types" $ do
          let leafInt = Pattern { value = 42, elements = [] }
          let leafString = Pattern { value = "test", elements = [] }
          elements leafInt `shouldBe` ([] :: [Pattern Int])
          elements leafString `shouldBe` ([] :: [Pattern String])
      
      describe "Edge cases" $ do
        
        it "leaf pattern with explicitly empty list of children behaves correctly" $ do
          let leaf = Pattern { value = "node", elements = [] }
          value leaf `shouldBe` "node"
          elements leaf `shouldBe` ([] :: [Pattern String])
          null (elements leaf) `shouldBe` True
        
        it "multiple leaf patterns with same value type can be created independently" $ do
          let leaf1 = Pattern { value = "node1", elements = [] }
          let leaf2 = Pattern { value = "node2", elements = [] }
          value leaf1 `shouldBe` "node1"
          value leaf2 `shouldBe` "node2"
          elements leaf1 `shouldBe` ([] :: [Pattern String])
          elements leaf2 `shouldBe` ([] :: [Pattern String])
        
        it "leaf patterns with different value types are type-safe" $ do
          let leafString = Pattern { value = "text", elements = [] }
          let leafInt = Pattern { value = 123, elements = [] }
          value leafString `shouldBe` "text"
          value leafInt `shouldBe` (123 :: Int)
          -- Type system ensures they cannot be mixed
    
    describe "Patterns with Children (User Story 2)" $ do
      
      describe "Creating patterns with children" $ do
        
        it "creates a pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          value parent `shouldBe` "parent"
          length (elements parent) `shouldBe` 1
          head (elements parent) `shouldBe` child
        
        it "creates a pattern with multiple children" $ do
          let child1 = Pattern { value = "child1", elements = [] }
          let child2 = Pattern { value = "child2", elements = [] }
          let child3 = Pattern { value = "child3", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          value parent `shouldBe` "parent"
          length (elements parent) `shouldBe` 3
          elements parent `shouldBe` [child1, child2, child3]
      
      describe "Value field accessor for patterns with children" $ do
        
        it "returns the correct value for pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          value parent `shouldBe` "parent"
        
        it "returns the correct value for pattern with multiple children" $ do
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let parent = Pattern { value = "root", elements = [child1, child2] }
          value parent `shouldBe` "root"
        
        it "returns the correct value for pattern with integer value and children" $ do
          let child = Pattern { value = 10, elements = [] }
          let parent = Pattern { value = 100, elements = [child] }
          value parent `shouldBe` (100 :: Int)
      
      describe "Elements field accessor for patterns with children" $ do
        
        it "returns correct child list for pattern with single child" $ do
          let child = Pattern { value = "child", elements = [] }
          let parent = Pattern { value = "parent", elements = [child] }
          elements parent `shouldBe` [child]
        
        it "returns correct child list for pattern with multiple children" $ do
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let child3 = Pattern { value = "c3", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          elements parent `shouldBe` [child1, child2, child3]
        
        it "returns correct child list preserving order" $ do
          let child1 = Pattern { value = "first", elements = [] }
          let child2 = Pattern { value = "second", elements = [] }
          let child3 = Pattern { value = "third", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          let children = elements parent
          value (head children) `shouldBe` "first"
          value (children !! 1) `shouldBe` "second"
          value (children !! 2) `shouldBe` "third"
      
      describe "Child elements accessibility and order" $ do
        
        it "child elements are accessible in correct order" $ do
          let child1 = Pattern { value = "a", elements = [] }
          let child2 = Pattern { value = "b", elements = [] }
          let child3 = Pattern { value = "c", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2, child3] }
          let children = elements parent
          children `shouldBe` [child1, child2, child3]
          map value children `shouldBe` ["a", "b", "c"]
        
        it "can access individual children by index" $ do
          let child1 = Pattern { value = "first", elements = [] }
          let child2 = Pattern { value = "second", elements = [] }
          let parent = Pattern { value = "parent", elements = [child1, child2] }
          let children = elements parent
          head children `shouldBe` child1
          last children `shouldBe` child2
      
      describe "Edge cases" $ do
        
        it "pattern with zero children behaves like leaf pattern" $ do
          let pattern = Pattern { value = "node", elements = [] }
          value pattern `shouldBe` "node"
          elements pattern `shouldBe` ([] :: [Pattern String])
          null (elements pattern) `shouldBe` True
        
        it "deeply nested patterns (multiple levels)" $ do
          let level3 = Pattern { value = "level3", elements = [] }
          let level2 = Pattern { value = "level2", elements = [level3] }
          let level1 = Pattern { value = "level1", elements = [level2] }
          let root = Pattern { value = "root", elements = [level1] }
          value root `shouldBe` "root"
          length (elements root) `shouldBe` 1
          value (head (elements root)) `shouldBe` "level1"
          let l1Children = elements (head (elements root))
          length l1Children `shouldBe` 1
          value (head l1Children) `shouldBe` "level2"
          let l2Children = elements (head l1Children)
          length l2Children `shouldBe` 1
          value (head l2Children) `shouldBe` "level3"
        
        it "pattern containing pattern containing pattern (arbitrary depth)" $ do
          let innermost = Pattern { value = "innermost", elements = [] }
          let middle = Pattern { value = "middle", elements = [innermost] }
          let outer = Pattern { value = "outer", elements = [middle] }
          value outer `shouldBe` "outer"
          value (head (elements outer)) `shouldBe` "middle"
          value (head (elements (head (elements outer)))) `shouldBe` "innermost"
        
        it "patterns with varying numbers of children (zero, one, many)" $ do
          let zeroChildren = Pattern { value = "zero", elements = [] }
          let oneChild = Pattern { value = "one", elements = [Pattern { value = "child", elements = [] }] }
          let child1 = Pattern { value = "c1", elements = [] }
          let child2 = Pattern { value = "c2", elements = [] }
          let child3 = Pattern { value = "c3", elements = [] }
          let manyChildren = Pattern { value = "many", elements = [child1, child2, child3] }
          length (elements zeroChildren) `shouldBe` 0
          length (elements oneChild) `shouldBe` 1
          length (elements manyChildren) `shouldBe` 3
