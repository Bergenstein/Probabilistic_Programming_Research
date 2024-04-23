> module ProbabilisticComputation where 

Reference: This code file is just reimplementation of this paper: [1]

> import Data.List (delete, (\\))
> import NondeterministicComputation 

> type Prob = Double

> class Monad m => MonadProb m where 
>   choice :: Prob -> m a -> m a -> m a 

> class (MonadAlt m, MonadProb m) => MonadAltProb m where 




(<!<) ::  (this operators mean) P (<!<) Q (>!>) R = P & Q v Q' & R from Hoare paper [2] but with slighly different notation: 

infixl 7 <!<
-- Define the custom operator for probabilistic choice
infixl 7 <!<

> (<!<) :: MonadProb m => m a -> Prob -> m a -> m a
> (<!<) mx p my = choice p mx my

> uniform :: MonadProb m => [a] -> m a
> uniform []     = error "cannot call uniform if list is empty"
> uniform [x]    = return x
> uniform (x:xs) = (return x) <!< (1 / fromIntegral (length (x:xs))) $ uniform xs

The Monty Hall Problem 

> data Door = A | B | C deriving (Eq, Show)


> doors :: [Door]
> doors = [A, B, C]

1. The host hides the car behind one of the doors randomly

> hide :: MonadProb m => m Door 
> hide = uniform doors 

2. the guest picks one of the doors randomly

> pick :: MonadProb m => m Door 
> pick = uniform doors 

3. The host opens another door (not where the car is or where the guest has picked) where there is a goat chilling 

> goat :: MonadProb m => Door -> Door -> m Door 
> goat f p = uniform (doors \\ [f, p])

> switch :: MonadProb m => Door -> Door -> m Door 
> switch p t = return (head (doors \\ [p, t]))

> stick :: MonadProb m => Door -> Door -> m Door 
> stick p t = return p 

p and t are known but f isn't unknow 

> play :: MonadProb m => (Door -> Door -> m Door) -> m Bool 
> play strategy = do 
>                   f <- hide         --host hides car behind door f 
>                   p <- pick         --guest picks door p 
>                   g <- goat f p     --host teases guest with door t (!= f, p)
>                   s <- strategy p g --we make a choice based on p and g 
>                   return ( s == f)  -- if guests choice is same as f where car is => WIN

ghci> let play switch = uniform [True, True, True]
ghci> let play stick = uniform [False, False, False] 

> pair :: MonadProb m => [a] -> [b] -> m (a, b)
> pair x y = uniform (cp x y) where
>   cp :: [a] -> [b] -> [(a, b)]
>   cp xa yb = [(a, b) | a <- xa, b <- yb]

> coin :: MonadProb m => m Bool 
> coin = return True <!< (1/2) $ return False 

> arbitrary :: MonadAlt m => m Bool
> arbitrary = return True <<>> return False 

> arbitraryCoin, coinArbitrary :: MonadAltProb m => m Bool
> arbitraryCoin = do {a <- arbitrary; c <- coin; return (a == c)}
> coinArbitrary = do {c <- coin; a <- arbitrary; return (a == c)}


Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html

2. C. A. R. HOARE [A Couple of Novelties in the Propositional Calculus] https://onlinelibrary.wiley.com/doi/abs/10.1002/malq.19850310905

