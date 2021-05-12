module DSException (Empty(Empty)
    , DuplicateElement(DuplicateElement)
    , NotFound(NotFound)
    , module Control.Exception) where
    import Control.Exception (throw, Exception)
      
    -- Define Empty Exception
    newtype Empty = Empty String
    instance Show Empty where
        show (Empty func) = "*** " ++ func ++ ": empty"
    instance Exception Empty
    
    newtype DuplicateElement = DuplicateElement String
    instance Show DuplicateElement where
        show (DuplicateElement func) = "*** " ++ func ++ ": this already has a duplicated element."
    instance Exception DuplicateElement
    
    newtype NotFound = NotFound String
    instance Show NotFound where
        show (NotFound func) = "*** " ++ func ++ ": cannot find"
    instance Exception NotFound