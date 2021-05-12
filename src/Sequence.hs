module Sequence (Sequence, empty, isEmpty, cons, Sequence.head, Sequence.tail,
    SequencePlus, (Sequence.++), update, suffixes, Stack(Nil, Cons)) where
    
    import Control.Exception(throw, ArrayException(IndexOutOfBounds))
    import DSException (Empty(Empty))
    
    -- Define Sequence Class
    class Sequence seq where
        empty :: seq a
        isEmpty :: seq a -> Bool
        
        cons :: a -> seq a -> seq a
        head :: seq a -> a
        tail :: seq a -> seq a
    
    -- Define SequencePlus Class  
    class (Sequence seq) => SequencePlus seq where
        infix 3 ++
        (++) :: seq a -> seq a -> seq a
        xs ++ ys = if isEmpty xs then ys else cons (Sequence.head xs) (Sequence.tail xs Sequence.++ ys)
        
        update :: Integral n => seq a -> n -> a -> seq a
        update xs _ _ | isEmpty xs = throw $ IndexOutOfBounds ""
        update xs 0 y = cons y $ Sequence.tail xs
        update xs i y = cons (Sequence.head xs) 
            $ update (Sequence.tail xs) (i - 1) y 
          
        suffixes :: seq a -> [seq a]
        suffixes xs | isEmpty xs = [empty]
        suffixes xs = xs : suffixes (Sequence.tail xs)
      
    -- Define instance of List  
    instance Sequence [] where
        empty = []
        isEmpty = null
        
        cons x = (x:)
        head = Prelude.head
        tail = Prelude.tail    
    
    instance SequencePlus [] where
        (++) = (Prelude.++)  
    
    -- Define Stack  
    data Stack a = Nil | Cons a (Stack a)
        deriving (Show, Eq)
    
    instance Sequence Stack where
        empty = Nil
        isEmpty Nil = True
        isEmpty _ = False
        
        cons = Cons
        head Nil = throw (Empty "Stack.head")
        head (Cons x _) = x
        tail Nil = throw (Empty "Stack.tail")
        tail (Cons _ xs) = xs
    
    instance SequencePlus Stack