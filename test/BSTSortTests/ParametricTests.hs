{-# LANGUAGE TemplateHaskell #-}
module BSTSortTests.ParametricTests(
  parametricTests
  ) where

-- tested module
import BSTSort

-- tests deps
import Test.QuickCheck.All
import Test.QuickCheck


prop_Sorted :: Ord a => [a] -> Bool
prop_Sorted list = is_sorted (sort $ list)
  where
  -- check if list is sorted
  is_sorted [] = True
  is_sorted [x] = True
  is_sorted (x1:x2:xs)
      | x1 <= x2 = is_sorted xs
      | otherwise = False

return []
parametricTests = $quickCheckAll
