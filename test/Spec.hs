import Tree
import Control.Monad
import qualified FiniteMap as FM

m = foldr (uncurry FM.bind) (BinaryTreeWithKey Nil)
    $ reverse [(1, 9), (6, 7), (4, 1), (2, 7), (3, 8), (8, 3), (5, 2), (7, 3), (1, 5), (3, 2), (6, 7)]

main :: IO ()
main = forM_ [0..9] (\key -> print $ Tree.lookup key m)