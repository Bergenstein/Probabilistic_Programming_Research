> module TowerOfHanoi where 

Reference: This code file is just reimplementation of this paper: [1]
% > import Monadic_Equational_Reasoning 

> skip :: Monad m => m ()
> skip = return ()


This represents a single effect of counting 

> class Monad m => MonadCount m where 
>   tick :: m ()

> hanoi :: MonadCount m => Int -> m ()
> hanoi 0   = skip 
> hanoi n = hanoi n >> tick >> hanoi (n-1) 

> rep :: Monad m => Int -> m () -> m ()
> rep 0 mx = skip
> rep n mx = mx >> rep (n-1) mx


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html
