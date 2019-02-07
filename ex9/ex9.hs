{- Zad. 1 -}

f :: [Integer] -> [Integer]
f (x:xs) = [n | n <- xs, n `mod` x /= 0]

primes :: [Integer]
primes = map head (iterate f [2..]) 

{- Zad. 2 -}

primes' :: [Integer]
primes' = 2 : [p | p <- [3..], all (\q -> p `mod` q /= 0) (takeWhile (\q -> q*q <= p) primes')]

{- Zad. 3 -}

