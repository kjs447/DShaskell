{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Set where
    class Ord a => Set set a where
        empty :: set a
        isEmpty :: set a -> Bool
        isEmpty xs = case xs of
          empty -> True
          _ -> False
        
        insert :: a -> set a -> set a
        member :: a -> set a -> Bool
