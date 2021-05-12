{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Set where
    import qualified Tree as T
    import DSException

    class (Ord a, Eq (set a)) => Set set a where
        empty :: set a
        isEmpty :: set a -> Bool
        isEmpty = (==) empty
        
        insert :: a -> set a -> set a
        member :: a -> set a -> Bool

    instance (Ord a) => Set T.BinaryTree a where
        empty = empty
        isEmpty = isEmpty

        insert x T.Nil = T.Node T.Nil x T.Nil
        insert x ys@(T.Node a y b)
            | isEmpty cand = goLeftAndAppend x (T.Node a y b)
            | val /= x = goLeftAndAppend x b
            | otherwise = throw $ DuplicateElement "Set.insert"
            where
                cand = T.getNodeCandidate x ys T.Nil
                T.Node _ val _ = cand
                goLeftAndAppend x T.Nil = insert x T.Nil
                goLeftAndAppend x (T.Node a y b) = T.Node (goLeftAndAppend x a) y b

        member _ T.Nil = False
        member x ys = not (isEmpty cand) && (val == x)
            where
                cand = T.getNodeCandidate x ys T.Nil
                T.Node _ val _ = cand