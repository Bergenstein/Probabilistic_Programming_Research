> module Monadic_Equational_Reasoning where 

Reference: This code file is just reimplementation of this paper: [1]

Monadic Laws:

1st Law: return x >>= f = f x  

returning a value and feeding it into a monadic function, f, will yield the same result as simply applying monadic function f to some value x

2nd Law: mx >>= return  = mx 

feeding results of a monadic computation into return is the same as simply performing the monadic computation 

1st and 2nd laws: return is the identity for >>= operator 

3rd Law: (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g)) -- >>= operator is associative 

> skip     :: Monad m => m ()
> skip     = return ()

> (>>)     :: Monad m => m a -> m b -> m b 
> mx >> my = mx >>= const my

> liftM :: Monad m => (a -> b) -> m a -> m b 
> liftM f mx = mx >>= return . f 

liftM satisfies map laws of a functor due to unit and associativity properties

liftM if      = id 
liftM (f . g) = liftM f . liftM g

> pair :: Monad m => (m a, m a) -> m (a, a)
> pair (mx, my) = mx >>= \x -> my >>= \y -> return (x,y)

Alternatively 

> pair' :: Monad m => (m a, m a) -> m (a, a)
> pair' (mx, my) = do {x <- mx; y <- my; return (x,y)}

do { y <- return x; f x}      = do {f x}
do { x <- ms; return x}       = do {mx}
do { x <- mx; y <- f x; f' y} = do {y <- do { x <- mx; f x}; f' y}

Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html