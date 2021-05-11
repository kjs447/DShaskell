{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Tree (BinaryTree(Nil, Node), empty, isEmpty, S.insert, S.member, complete) where
    import DSException (DuplicateElement(DuplicateElement), NotFound(NotFound), throw)
    import qualified Set as S
    import qualified FiniteMap as FM
    
    data BinaryTree a
        = Nil | Node (BinaryTree a) a (BinaryTree a)
        deriving (Show, Eq)

    empty :: BinaryTree a
    empty = Nil

    isEmpty :: BinaryTree a -> Bool
    isEmpty x = case x of
      empty -> True
      _ -> False
        
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

    instance (Ord a) => S.Set BinaryTree a where
        empty = empty
        isEmpty = isEmpty

        insert x Nil = Node Nil x Nil
        insert x ys@(Node a y b)
            | isEmpty cand = goLeftAndAppend x (Node a y b)
            | val /= x = goLeftAndAppend x b
            | otherwise = throw $ DuplicateElement "Set.insert"
            where
                cand = getNodeCandidate x ys Nil
                Node _ val _ = cand
                goLeftAndAppend x Nil = S.insert x Nil
                goLeftAndAppend x (Node a y b) = Node (goLeftAndAppend x a) y b

        member _ Nil = False
        member x ys = not (isEmpty cand) && (val == x)
            where
                cand = getNodeCandidate x ys Nil
                Node _ val _ = cand
                
    newtype TupleWithKey a b = TupleWithKey { getTuple :: (a, b) }
    instance (Eq a) => Eq (TupleWithKey a b) where
        TupleWithKey (x, _) == TupleWithKey (y, _) = x == y
    instance (Ord a) => Ord (TupleWithKey a b) where
        compare (TupleWithKey (x, _)) (TupleWithKey (y, _)) = compare x y
                
    newtype BinaryTreeWithKey key a = BinaryTreeWithKey { getTree :: BinaryTree (key, a) }
    instance Ord key => FM.FiniteMap BinaryTreeWithKey key a where
        empty = BinaryTreeWithKey Nil

        bind k v (BinaryTreeWithKey Nil) = BinaryTreeWithKey $ Node Nil (k, v) Nil
        bind k v (BinaryTreeWithKey m@(Node a y b)) = BinaryTreeWithKey $ case cmp of
            LT -> Node (getTree (FM.bind k v $ BinaryTreeWithKey a)) y b
            EQ -> Node a (k, v) b
            GT -> Node a y $ getTree (FM.bind k v $ BinaryTreeWithKey b)
            where cmp = compare k (fst y)

        lookup _ (BinaryTreeWithKey Nil) = throw $ NotFound "FiniteMap.lookup"
        lookup k (BinaryTreeWithKey m@(Node a y b)) = if isEmpty cand || k' /= k
            then throw $ NotFound "FiniteMap.lookup"
            else v
            where
                cand = getNodeCandidate (k, snd y) m Nil
                Node _ (k', v) _ = cand
     
