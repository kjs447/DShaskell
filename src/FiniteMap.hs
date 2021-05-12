{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FiniteMap where
    import qualified Tree as T

    class Ord key => FiniteMap map key a where
        empty :: map key a
        bind :: key -> a -> map key a -> map key a
        lookup :: key -> map key a -> Maybe a

    instance (Ord key) => FiniteMap T.BinaryTreeWithKey key a where
        empty = T.BinaryTreeWithKey T.Nil

        bind k v (T.BinaryTreeWithKey T.Nil) = T.BinaryTreeWithKey $ T.Node T.Nil (T.TupleWithKey (k, v)) T.Nil
        bind k v (T.BinaryTreeWithKey (T.Node a y b)) = T.BinaryTreeWithKey $ case cmp of
            LT -> T.Node (T.getTree (bind k v $ T.BinaryTreeWithKey a)) y b
            EQ -> T.Node a (T.TupleWithKey (k, v)) b
            GT -> T.Node a y $ T.getTree (bind k v $ T.BinaryTreeWithKey b)
            where cmp = compare k (fst $ T.getTuple y)

        lookup k m
            | T.isEmpty (T.getTree m) || T.isEmpty cand || k' /= k
                = Nothing
            | otherwise = Just v
            where
                T.Node _ (T.TupleWithKey (_, y)) _ = T.getTree m
                cand = T.getNodeCandidate (T.TupleWithKey (k, y)) (T.getTree m) T.Nil
                T.Node _ (T.TupleWithKey (k', v)) _ = cand

