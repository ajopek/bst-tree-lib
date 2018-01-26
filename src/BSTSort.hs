{-|
  Module      : BSTSort
  Description : Module provides basic sorting using BST trees functionality
-}


module BSTSort
    (
      sort
    ) where

    import BSTree

    sort :: Ord k => [k] -> [k]
    sort list =
      (inorderBST . insertList) $ list
