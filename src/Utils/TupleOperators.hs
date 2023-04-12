module Utils.TupleOperators ((<->)) where
import Data.List (minimumBy)


(<->) :: (t -> a) -> (t -> b) -> t -> (a, b)
f <-> g  = \x -> (f x, g x)