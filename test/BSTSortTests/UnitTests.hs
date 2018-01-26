module BSTSortTests.UnitTests (
    unitTests
    ) where

-- tested module
import BSTSort

-- tests deps
import Test.HUnit

test_sortList = TestCase (do
  assertEqual "List not sorted" [1::Int, 2::Int, 3::Int] (sort [2::Int, 3::Int, 1::Int])
  )

test_sortSortedList = TestCase (do
  assertEqual "Sort broke sorted list" [1::Int, 2::Int, 3::Int] (sort [1::Int, 2::Int, 3::Int])
  )
unitTests = TestList [test_sortSortedList, test_sortList]
