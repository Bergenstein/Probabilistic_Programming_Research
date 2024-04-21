> module Prob where 

> import Data.List (delete, (\\))

> newtype Probability = P Float 
>   deriving Show 
> newtype Dist a = D {unD :: [(a, Probability)]}
>   deriving Show 

> data Color = R | G | B deriving (Eq, Show)


> type Spread a = [a] -> Dist a 

> type Trans a = a -> Dist a 

> uniform :: [a] -> Dist a
> uniform xs = D $ map (\x -> (x, P (1 / fromIntegral (length xs)))) xs

> die = uniform [1..6]

> type Event a = a -> Bool 

> (??) :: Event a -> Dist a -> Probability 
> (??) p = P . sum . map (unP . snd) . filter (p . fst) . unD 
>   where unP (P x) = x

P . sum . map (unP . snd) . filter (p . fst) . unD 

> joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c 
> joinWith f (D d) (D d') = D [(f x y, P (unP p * unP q))| (x,p) <- d, (y,q) <- d'] 
>   where unP (P x) = x

> prod :: Dist a -> Dist b -> Dist (a,b)
> prod = joinWith(,)

> certainly :: a -> Dist a
> certainly x = D [(x, P 1)]

> dist :: Int -> Dist [Int]
> dist 0 = certainly []
> dist n = joinWith (:) die (dist (n-1))

> instance Functor Dist where
>   fmap f (D d) = D [(f x, p) | (x, p) <- d]

> instance Applicative Dist where
>   pure x             = D [(x, P 1)]
>   (D fs) <*> (D xs)  = D [(f x, P (unP pf * unP px)) | (f, pf) <- fs, (x, px) <- xs]
>      where unP (P x) = x

> instance Monad Dist where 
>   return             = pure 
>   (D d) >>= f        = D [(y, P (unP q * unP p)) | (x,p) <- d, (y,q) <- unD (f x)]
>      where unP (P x) = x

> instance MonadFail Dist where 
>   fail _ = D []

> (>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 
> f >@> g = (>>= g) . f 

> sequ :: Monad m => [a -> m a] -> a -> m a 
> sequ = foldl (>@>) return

> selectOne :: Eq a => [a] -> Dist (a, [a])
> selectOne c = uniform [(v, delete v c) | v <- c]

> selectMany :: Eq a => Int -> [a] -> Dist ([a], [a])
> selectMany 0 c = return ([], c)
> selectMany n c = do (x, c1) <- selectOne c 
>                     (xs, c2) <- selectMany (n-1) c1
>                     return (x:xs, c2)

> mapD :: (a -> b) -> Dist a -> Dist b 
> mapD = fmap 

> select :: Eq a => Int -> [a] -> Dist [a]
> select n = mapD (reverse . fst) . selectMany n

ghci> ((>=2) . length . filter (==6)) ?? dist 4
P 0.13194445

ghci> (==[R,G,B]) ?? select 3 [R, R, G, G, B] 
P 6.666667e-2

> data Outcome = Win | Lose deriving Show 
> firstChoice :: Dist Outcome 
> firstChoice = uniform [Win, Lose, Lose]

> switch :: Trans Outcome 
> switch Win  = certainly Lose 
> switch Lose = certainly Win 


ghci> firstChoice 
D {unD = [(Win,P 0.33333334),(Lose,P 0.33333334),(Lose,P 0.33333334)]}
ghci> firstChoice >>= switch 
D {unD = [(Lose,P 0.33333334),(Win,P 0.33333334),(Win,P 0.33333334)]}

> data Door = A' | B' | C' deriving (Show, Enum, Eq)

> doors = [A' .. C']

> data State = Doors {prize :: Door, chosen :: Door, opened :: Door} deriving Show 

> start :: State 
> start = Doors {prize = u, chosen = u, opened = u} where 
>   u = undefined 

> hide :: Trans State 
> hide s = uniform [s{prize=d} | d <- doors]

> choose :: Trans State 
> choose s = uniform [s{chosen=d} | d <- doors]

> open :: Trans State 
> open s = uniform [s{opened=d} | d <- doors \\ [prize s, chosen s]]

> type Strategy = Trans State 
> switch' :: Strategy
> switch' s = uniform [s{chosen=d} | d <- doors \\ [chosen s, opened s]]

> stay :: Strategy 
> stay = certainlyT id 

> certainlyT :: (a -> a) -> Trans a 
> certainlyT f = certainly . f 

> game :: Strategy -> Trans State 
> game s = sequ [hide, choose, open, s]

> result :: State -> Outcome 
> result s | chosen s == prize s = Win
>          | otherwise           = Lose

> eval :: Strategy -> Dist Outcome 
> eval s = mapD result (game s start)

ghci> eval stay 
D {unD = [(Win,P 5.555556e-2),(Win,P 5.555556e-2),(Lose,P 0.11111112),(Lose,P 0.11111112),(Lose,P 0.11111112),(Win,P 5.555556e-2),(Win,P 5.555556e-2),(Lose,P 0.11111112),(Lose,P 0.11111112),(Lose,P 0.11111112),(Win,P 5.555556e-2),(Win,P 5.555556e-2)]}

ghci> eval switch'
D {unD = [(Lose,P 5.555556e-2),(Lose,P 5.555556e-2),(Win,P 0.11111112),(Win,P 0.11111112),(Win,P 0.11111112),(Lose,P 5.555556e-2),(Lose,P 5.555556e-2),(Win,P 0.11111112),(Win,P 0.11111112),(Win,P 0.11111112),(Lose,P 5.555556e-2),(Lose,P 5.555556e-2)]}
ghci> 

Appendix:

1. M. ERWIG, S. KOLLMANSBERGER: [Probabilistic Functional Programming in Haskell]. (Last Accessed 21 April 2024) https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf


