module BSTreeTests.UnitTests(
    unitTests
    ) where

-- tested module
import BSTree
import BSTree.Internal

-- tests deps
import Test.HUnit

validTree =
    (Node
    (Node (Leaf) 3 (Leaf))
    5
    (Node (Leaf) 7 (Leaf))
    )

anotherTree =
    (Node
    (Node (Leaf) 3 (Leaf))
    6
    (Node (Leaf) 8 (Leaf))
    )

invalidTree =
    (Node
    (Node (Leaf) 3 (Leaf))
    1
    (Node (Leaf) 7 (Leaf))
    )


test_isValidValidTree = TestCase (do
  assertEqual "Valid tree is not deteced as valid" True (isValid validTree)
  )

test_isValidInvalidTree = TestCase (do
  assertEqual "Invalid tree is not deteced as invalid" False (isValid invalidTree)
  )

test_containFive = TestCase (do
  assertEqual "Tree with key equal 5 return false" True (contains validTree 5)
  )

test_minKey = TestCase (do
  assertEqual "Minimum key in tree is not equal 3" 3 (leftBST validTree)
  )

test_maxKey = TestCase (do
  assertEqual "Maximum key in tree is not equal 7" 7 (rightBST validTree)
  )

test_compareOfTrees = TestCase (do
  assertEqual "Diffrent trees are equal" False (compareBST validTree anotherTree)
  )

test_heightTree = TestCase (do
  assertEqual "Height of tree is not equal 2" 2 (heightBST validTree)
  )

unitTests = TestList [
           test_isValidValidTree,
           test_isValidInvalidTree,
           test_containFive,
           test_minKey,
           test_maxKey,
           test_compareOfTrees,
           test_heightTree
           ]
