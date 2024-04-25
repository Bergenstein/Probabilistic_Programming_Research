> module TreeRelabelling where 

Reference: This code file is just reimplementation of this paper: [1]. This paper provides a different solution to the original problem presented by Hutton and Fulger: [2]

> import NondeterministicComputation 
> import ProbabilisticComputation 

> data Tree a = Tip a | Bin ((Tree a, Tree a))

> newtype Symbol = Symbol Int deriving (Eq)

> class Monad m => MonadFresh m where 
>   fresh :: m Symbol 

> symbols :: MonadFresh m => Int -> m [Symbol]
> symbols n = sequence (replicate n fresh)

> class (MonadFresh m, MonadFail' m) => MonadFreshFail m

Some laws:

1. assert distinct . symbols = symbols 

The only proprty we will require of the predicate distict is that it is segment-closed. Predicate p is segment closed if: p (x ++ y) = p x & p y 

> foldt :: (a -> b) -> ((b,b) -> b) -> Tree a -> b 
> foldt f g (Tip a)   = f a 
> foldt f g (Bin (t,u)) = g (foldt f g t, foldt f g u)

relabel :: MonadFresh m => Tree a -> m (Tree Symbol)
relabel = foldt (M Tip . const fresh) (M Bin . pair)

> q :: Eq a => (a, a) -> Bool
> q (x, y) = x /= y

> conCat :: ([a], [a]) -> [a]
> conCat = uncurry (++)


> pair' :: Monad m => m [Symbol] -> m [Symbol] -> m ([Symbol], [Symbol])
> pair' mx my = do
>     x <- mx
>     y <- my
>     return (x, y)


> joitLst :: Monad m => (m [Symbol], m [Symbol]) -> m [Symbol]
> joitLst (mx, my) = do
>     x <- mx
>     y <- my
>     return (conCat (x, y))
   


> relabel :: MonadFresh m => Tree a -> m (Tree Symbol)
> relabel = foldt tipCase binCase
>    where
>    tipCase a = fmap Tip fresh     
>    binCase (left, right) = do       
>       l <- left
>       r <- right
>       return (Bin (l, r))


> dlabel :: MonadFail m => Tree Symbol -> m [Symbol]
> dlabel = foldt (return . wrap) joitLst where
>     wrap a = [a]

> size :: Tree a -> Int
> size = foldt (const 1) (\(x, y) -> x + y)


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html
2. G. Hutton, D. Fulger [Reasoning About Effects: Seeing the Wood Through the Trees]: https://www.researchgate.net/publication/237793722_Reasoning_About_Effects_Seeing_the_Wood_Through_the_Trees_Extended_Version


