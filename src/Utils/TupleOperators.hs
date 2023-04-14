module Utils.TupleOperators ((<->), off, minBy) where
import Data.List (minimumBy)
import Data.Function (on)


(<->) :: (t -> a) -> (t -> b) -> t -> (a, b)
f <-> g  = \x -> (f x, g x)

off :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
(f `off` g) x y = f (g x y)

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f a b = minimumBy (compare `on` f) [a, b]