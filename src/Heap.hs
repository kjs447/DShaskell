{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Heap where
    import qualified Tree as T

    class (Eq (heap a), Ord a) => MinHeap heap a where
        empty :: heap a
        isEmpty :: heap a -> Bool
        isEmpty = (==) empty

        insert :: a -> heap a -> heap a
        merge :: heap a -> heap a -> heap a

        findMin :: heap a -> a
        deleteMin :: heap a -> heap a
