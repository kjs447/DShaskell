{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module FiniteMap where
    class Ord key => FiniteMap map key a where
        empty :: map key a
        bind :: key -> a -> map key a -> map key a
        lookup :: key -> map key a -> a
        
        