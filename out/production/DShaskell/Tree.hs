{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tree (BinaryTree(Nil, Node), Set, empty, isEmpty, insert, member) where
    import DSException (DuplicateElement(DuplicateElement), throw)
    
    data BinaryTree a
        = Nil | Node (BinaryTree a) a (BinaryTree a)
        deriving Show
        
    getNodeCandidate :: Ord a => a -> BinaryTree a -> BinaryTree a -> BinaryTree a
    getNodeCandidate _ Nil key = key
    getNodeCandidate x node@(Node a y b) key = if x < y
      then getNodeCandidate x a key
      else getNodeCandidate x b node
        
    class Ord a => Set set a where
        empty :: set a
        isEmpty :: set a -> Bool
        isEmpty xs = case xs of
          empty -> True
          _ -> False
        
        insert :: a -> set a -> set a
        member :: a -> set a -> Bool

    instance (Ord a) => Set BinaryTree a where
        empty = Nil

        insert x Nil = Node Nil x Nil
        insert x (Node a y b)
            | x < y = Node (insert x a) y b
            | x > y = Node a y (insert x b)
            | otherwise = throw $ DuplicateElement "Set.insert"
            
        
        member _ Nil = False
        member x ys = not (isEmpty cand) && (val == x) 
            where
                cand = getNodeCandidate x ys Nil
                Node _ val _ = cand
                
        
                
    
     
