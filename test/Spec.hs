

import Test.HUnit
import Test.QuickCheck.All

import BSTree
import BSTSortTests.UnitTests as BSTSortUnit
import BSTSortTests.ParametricTests as BSTSortParametric
import BSTreeTests.UnitTests as BSTreeUnit
--main :: IO ()
main = do runTestTT BSTSortUnit.unitTests
          BSTSortParametric.parametricTests
          runTestTT BSTreeUnit.unitTests
