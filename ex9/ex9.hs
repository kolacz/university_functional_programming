{- Zad. 1 -}

f :: [Integer] -> [Integer]
f (x:xs) = [n | n <- xs, n `mod` x /= 0]

primes :: [Integer]
primes = map head (iterate f [2..]) 

{- Zad. 2 -}

primes' :: [Integer]
primes' = 2 : [p | p <- [3..], all (\q -> p `mod` q /= 0) (takeWhile (\q -> q*q <= p) primes')]

{- Zad. 3 -}

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

{- Zad. 4 -}

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x l@(y:ys) = (x:l) : (map (y:) (insert x ys))

iperm :: [a] -> [[a]]
iperm [] = [[]]
iperm (x:xs) = [p | p' <- iperm xs, p <- insert x p']

remove :: (Eq a) => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y then ys else y : (remove x ys)
  
sperm :: (Eq a) => [a] -> [[a]]
sperm [] = [[]]
sperm xs = [x:ys | x <- xs, ys <- sperm (remove x xs)]

{- Zad. 5 -}

sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = (map (x:) (sublist xs)) ++ (sublist xs)

{- Zad. 6 -}


