{-# LANGUAGE TemplateHaskell #-}
module BSTreeTests.ParametricTests (
--    parametricTests
    ) where

      -- tested module
import BSTree

-- tests deps
import Test.QuickCheck.All
import Test.QuickCheck


instance (Ord a, Arbitrary a) => Arbitrary (BST a) where
  arbitrary = sized $ (\n -> do
        list <- vector n
        let randomTree = insertList list
        return $ randomTree
      )

-- Checs valididty of generated tree
-- property_treeAfterInsertIsValid key tree =




--------------------------
return []
parametricTests :: IO Bool
parametricTests = $quickCheckAll
