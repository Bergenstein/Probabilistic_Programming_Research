> module ExceptionalComputation where 

Reference: This code file is just reimplementation of this paper: [1]


> import NondeterministicComputation 
> import Monadic_Equational_Reasoning 

> class MonadFail' m => MonadExcept m where 
>   catch :: m a -> m a -> m a -- the first computation might fail and if it does the second computation takes place. At the end, a monadic computation is returned, thanks to the handler (the second computation)

laws:

1. catch fail h         = h
2. catch m fail         = m 
3. catch m (catch h h') = catch (catch m h) h'

> fastProd :: MonadExcept m => [Int] -> m Int 
> fastProd xs = catch (work xs) (return 0)

> work :: MonadFail' m => [Int] -> m Int 
> work xs | 0 `elem` xs   = fail'
>         | otherwise = return (product xs) 

> work' :: MonadFail' m => [Int] -> m Int 
> work' = foldr next (return 1) where 
>   next n mx | n == 0    = fail' 
>             | otherwise = liftM (n *) mx 


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html