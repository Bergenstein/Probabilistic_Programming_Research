> module Permutation where 

Reference: This code file is just reimplementation of this paper: [1]

> import NondeterministicComputation

> select :: MonadNondet m => [a] -> m (a, [a])
> select []     = fail"empty list provided"
> select (x:xs) = return (x, xs) <<>> do 
>                               {(y,ys) <- select xs; return (y, x:ys)}


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html