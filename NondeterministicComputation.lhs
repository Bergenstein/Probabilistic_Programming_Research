> module NondeterministicComputation where 

> import TowerOfHanoi

Reference: This code file is just reimplementation of this paper: [1]

> class Monad m => MonadFail' m where 
>   fail' :: m a 

% > class MonadAlt m where 
% >   (<<>>) :: m a -> m a -> m a 

> class Monad m => MonadAlt m where
>   (<<>>) :: m a -> m a -> m a -- <<>> combines two monadic operations into one, encapsulating notion of having choices in computations. 

> instance MonadAlt Maybe where
>     Nothing <<>> my = my
>     mx <<>> Nothing = mx
>     Just x <<>> Just y = Just x

> instance MonadFail' Maybe where
>     fail' = Nothing


subject to:
1. associativity of operator <<>>: (m <<>> n) <<>> p = m <<>> (n <<>> p)
2. Leftwards composition distributivity of <<>>: (m <<>> n) >>= k = (m >>= k) <<>> (n >>= k)

> class (MonadFail m, MonadAlt m)=> MonadNondet m where 

> newtype List' a = List' { unList' :: [a] }

> instance Functor List' where
>    fmap f (List' xs) = List' (map f xs)

> instance Applicative List' where 
>    pure a = List' [a]
>    (List' fs) <*> (List' xs) = List' [f x | f <- fs, x <- xs]

> instance Monad List' where
>    return           = pure
>    (List' mx) >>= f = List' (concat (map (unList' . f) mx))

> instance MonadFail List' where
>    fail _ = List' []

> instance MonadAlt List' where 
>   (List' xs) <<>> (List' ys) = List' (xs ++ ys)


> guard :: MonadFail' m => Bool -> m ()
> guard b | b         = skip 
>         | otherwise = fail' 

> assert :: MonadFail' m => (a -> Bool) -> m a -> m a
> assert p mx = do {x <- mx; guard (p x); return x}


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html





