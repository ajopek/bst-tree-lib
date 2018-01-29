{-# OPTIONS_HADDOCK prune #-}
module BSTree.Internal where

      data BST a =
          Leaf
          | Node (BST a) a (BST a)
            deriving (Show, Read)

      -- | Check if BST conains key
      contains :: (Ord a) => (BST a) -> a -> Bool
      contains Leaf _ = False
      contains (Node tree1 value tree2) x
             | x == value = True
             | x < value  = contains tree1 x
             | x > value  = contains tree2 x

      -- | Insert key to tree

      insertBST :: (Ord a) => BST a -> a -> BST a
      insertBST Leaf x = Node Leaf x Leaf
      insertBST (Node tree1 value tree2) x
               | value == x = Node tree1 value tree2
               | value < x  = Node tree1 value (insertBST tree2 x)
               | value > x  = Node (insertBST tree1 x) value tree2

      -- | Most left element of tree (min)

      leftBST :: (Ord a) => BST a -> a
      leftBST (Node Leaf value _) = value
      leftBST (Node tree1 _ _)    = leftBST tree1

      -- | Most right element of tree (max)

      rightBST :: (Ord a) => BST a -> a
      rightBST (Node _ value Leaf) = value
      rightBST (Node _ _ tree2)    = rightBST tree2

      -- | Delete root from tree

      deleteRoot :: (Ord a) => BST a -> BST a
      deleteRoot (Node Leaf value tree2)  = tree2
      deleteRoot (Node tree1 value Leaf)  = tree1
      deleteRoot (Node tree1 value tree2) = (Node tree1 minTree (deleteFromBST tree2 minTree))
                                         where
                                              minTree = leftBST tree2
      -- | Delete key from tree

      deleteFromBST :: (Ord a) => BST a -> a -> BST a
      deleteFromBST Leaf _ = Leaf
      deleteFromBST (Node tree1 value tree2) x
               | x == value = deleteRoot (Node tree1 value tree2)
               | x < value  = Node (deleteFromBST tree1 x) value tree2
               | x > value  = Node tree1 value (deleteFromBST tree2 x)

      -- | Insert value from list to tree

      insertList :: (Ord a) => [a] -> BST a
      insertList []     = Leaf
      insertList (x:xs) = insertElement (Node Leaf x Leaf) xs
                where
                     insertElement xss []     = xss
                     insertElement xss (x:xs) = insertElement (insertBST xss x) xs

      -- | Convert to list in order

      inorderBST :: (Ord a) => BST a -> [a]
      inorderBST Leaf = []
      inorderBST (Node tree1 value tree2)
                = inorderBST tree1 ++ [value] ++ inorderBST tree2

      -- | Convert to list pre order

      preorderBST :: (Ord a) => BST a -> [a]
      preorderBST Leaf = []
      preorderBST (Node tree1 value tree2)
                 = [value] ++ preorderBST tree1 ++ preorderBST tree2

      -- | Convert to list post order

      postorderBST :: (Ord a) => BST a -> [a]
      postorderBST Leaf = []
      postorderBST (Node tree1 value tree2)
                  = postorderBST tree1 ++ postorderBST tree2 ++ [value]

      -- | Compare trees

      compareBST :: (Ord a) => BST a -> BST a -> Bool
      compareBST tree1 tree2 = (inorderBST tree1 == inorderBST tree2)

      instance Ord a =>  Eq (BST a) where
        t1 == t2 = compareBST t1 t2

      -- | Get height of BST

      heightBST :: (Ord a) => BST a -> Int
      heightBST Leaf                     = 0
      heightBST (Node tree1 value tree2) = 1 + max (heightBST tree1) (heightBST tree2)


      -- | Check if valid
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
