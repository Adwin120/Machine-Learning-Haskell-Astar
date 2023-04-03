module Utils.TupleOperators ((<:-:>), (-:>), (<:-)) where
(-:>) :: t -> (t -> b) -> (t, b)
x -:> f = (x, f x)

(<:-:>) :: (t -> a) -> (t -> b) -> t -> (a, b)
f <:-:> g = \x ->  (f x, g x)

(<:-) :: (t -> b) -> t -> (b, t)
f <:- x = (f x, x)