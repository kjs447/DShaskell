{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Tree where
    import DSException
    data BinaryTree a
        = Nil | Node (BinaryTree a) a (BinaryTree a)
        deriving Show

    instance Eq a => Eq (BinaryTree a) where
        Nil == Nil = True
        (Node l v r) == (Node l' v' r') = v == v' && l == l' && r == r'
        _ == _ = False

    getLeft :: BinaryTree a -> BinaryTree a
    getLeft Nil = throw $ Empty "BinaryTree.getLeft"
    getLeft (Node l _ _) = l

    getRight :: BinaryTree a -> BinaryTree a
    getRight Nil = throw $ Empty "BinaryTree.getRight"
    getRight (Node _ _ r) = r

    getValue :: BinaryTree a -> a
    getValue Nil = throw $ Empty "BinaryTree.getValue"
    getValue (Node _ x _) = x

    empty :: BinaryTree a
    empty = Nil

    isEmpty :: BinaryTree a -> Bool
    isEmpty Nil = True
    isEmpty _ = False
        
    getNodeCandidate :: Ord a => a -> BinaryTree a -> BinaryTree a -> BinaryTree a
    getNodeCandidate _ Nil key = key
    getNodeCandidate x node@(Node a y b) key = if x < y
      then getNodeCandidate x a key
      else getNodeCandidate x b node
      
    complete :: a -> Integer -> BinaryTree a
    complete x n
        | n == 0 = Nil
        | odd n = Node sharedNode x sharedNode
        | otherwise = Node (insertLastOne sharedNode (n `div` 2)) x sharedNode
            where 
                sharedNode = complete x ((n - 1) `div` 2)
                insertLastOne Nil _ = Node Nil x Nil
                insertLastOne tree indicator
                    | indicator == 1 = Node Nil x Nil
                    | indicator == 2 = Node (Node Nil x Nil) x r
                    | indicator == 3 = Node l x (Node Nil x Nil)
                    | even indicator = Node (insertLastOne l (indicator `div` 2)) x r
                    | otherwise = Node l x (insertLastOne r (indicator `div` 2))
                        where Node l x r = tree
                
    newtype TupleWithKey key a = TupleWithKey { getTuple :: (key, a) }
    instance (Show key, Show a) => Show (TupleWithKey key a) where
        show = show . getTuple
    instance (Eq key) => Eq (TupleWithKey key a) where
        TupleWithKey (x, _) == TupleWithKey (y, _) = x == y
    instance (Ord key) => Ord (TupleWithKey key a) where
        compare (TupleWithKey (x, _)) (TupleWithKey (y, _)) = compare x y
                
    newtype BinaryTreeWithKey key a = BinaryTreeWithKey { getTree :: BinaryTree (TupleWithKey key a) }
    instance (Show key, Show a) => Show (BinaryTreeWithKey key a) where
        show = show . getTree
    instance Eq key => Eq (BinaryTreeWithKey key a) where
        x == y = getTree x == getTree y
     
