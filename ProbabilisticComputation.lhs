> module ProbabilisticComputation where 

Reference: This code file is just reimplementation of this paper: [1]

> import Data.List (delete, (\\))
> import NondeterministicComputation 

> type Prob = Double

> class Monad m => MonadProb m where 
>   choice :: Prob -> m a -> m a -> m a -- mx with probability of p, and my with probability of p' (1-p)

class Monad m => MonadAlt m where
   (<<>>) :: m a -> m a -> m a -- <<>> combines two monadic operations into one, encapsulating notion of having choices in computations. 

> class (MonadAlt m, MonadProb m) => MonadAltProb m where -- probabilistic choice should distribute over nondeterministic choice 


The MonadProb introduces a choice function which takes a probability and two monadic operations. It then picks one of these monadic operations with the probability p to one and (1-p) to the other.

The MonadProb also introduces a (<!<) operation that captures the effects of choice. Takes a monadic operation, a probability float and another monadic operation. It then uses choice function on this sequence of operations: choice p mx my which will then return another monadic instance. 


(<!<) ::  (this operators mean) P (<!<) Q (>!>) R = P & Q v Q' & R from Hoare paper [2] but with slighly different notation: 

> (<!<) :: MonadProb m => m a -> Prob -> m a -> m a
> (<!<) mx p my = choice p mx my -- p is a double between 0<p<1

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
> goat f p = uniform (doors \\ [f, p]) -- not the door picked by guest or by host where car is hidden

> switch :: MonadProb m => Door -> Door -> m Door 
> switch p t = return (head (doors \\ [p, t])) 

> stick :: MonadProb m => Door -> Door -> m Door 
> stick p t = return p -- we stick with our original pick p 

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
> coin = return True <!< (1/2) $ return False -- a fair coin 

> arbitrary :: MonadAlt m => m Bool
> arbitrary = return True <<>> return False 

> arbitraryCoin, coinArbitrary :: MonadAltProb m => m Bool
> arbitraryCoin = do {a <- arbitrary; c <- coin; return (a == c)}
> coinArbitrary = do {c <- coin; a <- arbitrary; return (a == c)}

These allow the host make nondeterministic choice not a probabilistic choice. In other words, Monte is the boss and isn't playing by any rules. 

> arbitrary' :: (MonadAlt m, MonadFail' m) => [a] -> m a
> arbitrary' [] = fail' 
> arbitrary' xs = foldr (<<>>) fail' (map return xs)


> hide' :: (MonadAlt m, MonadFail' m) => m Door
> hide' = arbitrary' doors

> tease' :: (MonadAlt m, MonadFail' m) => Door -> Door -> m Door
> tease' f p = arbitrary' (doors \\ [f, p])

Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html

2. C. A. R. HOARE [A Couple of Novelties in the Propositional Calculus] https://onlinelibrary.wiley.com/doi/abs/10.1002/malq.19850310905

