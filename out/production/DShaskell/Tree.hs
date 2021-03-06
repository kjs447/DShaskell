{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Tree (BinaryTree(Nil, Node), getLeft, getRight, getValue, empty, isEmpty, S.insert, S.member, complete
    , TupleWithKey(TupleWithKey, getTuple), BinaryTreeWithKey(BinaryTreeWithKey, getTree), FM.bind, FM.lookup,
    getNodeCandidate) where
    import DSException (DuplicateElement(DuplicateElement), throw)
    import qualified Set as S
    import qualified FiniteMap as FM
    
    data BinaryTree a
        = Nil | Node (BinaryTree a) a (BinaryTree a)
        deriving Show

    instance Eq a => Eq (BinaryTree a) where
        Nil == Nil = True
        (Node l v r) == (Node l' v' r') = v == v' && l == l' && r == r'
        _ == _ = False

    getLeft (Node l x r) = l
    getRight (Node l x r) = r
    getValue (Node l x r) = x

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
        x == y = (getTree x) == (getTree y)

    instance (Ord key) => FM.FiniteMap BinaryTreeWithKey key a where
        empty = BinaryTreeWithKey Nil

        bind k v (BinaryTreeWithKey Nil) = BinaryTreeWithKey $ Node Nil (TupleWithKey (k, v)) Nil
        bind k v (BinaryTreeWithKey m@(Node a y b)) = BinaryTreeWithKey $ case cmp of
            LT -> Node (getTree (FM.bind k v $ BinaryTreeWithKey a)) y b
            EQ -> Node a (TupleWithKey (k, v)) b
            GT -> Node a y $ getTree (FM.bind k v $ BinaryTreeWithKey b)
            where cmp = compare k (fst $ getTuple y)


        lookup k m
            | isEmpty (getTree m) || isEmpty cand || k' /= k
                = Nothing
            | otherwise = Just v
            where
                Node _ (TupleWithKey (_, y)) _ = getTree m
                cand = getNodeCandidate (TupleWithKey (k, y)) (getTree m) Nil
                Node _ (TupleWithKey (k', v)) _ = cand
     
