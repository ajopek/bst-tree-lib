module BSTreeTests.UnitTests(
    unitTests
    ) where

-- tested module
import BSTree
import BSTree.Internal

-- tests deps
import Test.HUnit


isValid (Node left val right) = bigger right && smaller left
    where
      bigger (Node left x right)
          | x >= val = isValid left && isValid right
          | otherwise = False
      bigger (Leaf) = True
      smaller (Node left x right)
          | x <= val = isValid left && isValid right
          | otherwise = False
      smaller (Leaf) = True
isValid (Leaf) = True

validTree =
    (Node
    (Node (Leaf) 3 (Leaf))
    5
    (Node (Leaf) 7 (Leaf))
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

unitTests = TestList [test_isValidValidTree, test_isValidInvalidTree]
