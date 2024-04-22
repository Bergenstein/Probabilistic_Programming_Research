> module StatefulComputation where 

Reference: This code file is just reimplementation of this paper: [1]

> type Square a = (a,a)

> square :: (a -> b) -> (Square a -> Square b)
> square f (a, b) = (f a, f b)

> test :: (Int, Int) -> ([Int], [Int]) -> (Bool, ([Int], [Int]))
> test (c, r) (ups, downs) = (u `notElem` ups && d `notElem` downs, (u: ups, d: downs) ) where 
>    (u,d) = (r - c, r + c)

> safeOne :: [(Int, Int)] -> ([Int], [Int]) -> (Bool, ([Int], [Int]))
> safeOne crs updowns = foldr stepOne startOne' crs where
>     startOne' = startOne updowns

> startOne :: ([Int], [Int]) -> (Bool, ([Int], [Int]))
> startOne updowns = (True, updowns)

> stepOne :: (Int, Int) -> (Bool, ([Int], [Int])) -> (Bool, ([Int], [Int]))
> stepOne cr (restOK, updowns) = (thisOK && restOK, updowns') where 
>     (thisOK, updowns') = test cr updowns

Appendix:
1. J. Gibbon, R. Hinze [Just do it: Simple monadic equational reasoning]https://www.cs.ox.ac.uk/publications/publication4877-abstract.html