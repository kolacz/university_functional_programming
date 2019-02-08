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

(><) :: [a] -> [b] -> [(a, b)]
(><) xs (y:ys) = aux xs ys [y] where
  aux xs     [] acc =  zip xs acc
  aux xs (z:zs) acc = (zip xs acc) ++ (aux xs zs (z:acc)) 

pairs :: [(Integer, Integer)]
pairs = [0..] >< [0..]

{- Zad. 7 -}

data Tree a = Node (Tree a) a (Tree a) | Leaf

data Set a = Fin (Tree a) | Cofin (Tree a)

setInsert :: Ord a => a -> Set a -> Set a
setInsert x (Fin s) = Fin(treeInsert x s) where 
  treeInsert x Leaf = Node Leaf x Leaf
  treeInsert x s@(Node l y r) = if x < y then Node (treeInsert x l) y r else
                                if x > y then Node l y (treeInsert x r) else s 

setFromList :: Ord a => [a] -> Set a 
setFromList xs = foldr setInsert setEmpty xs

setEmpty :: Ord a => Set a
setEmpty = Fin(Leaf)

setFull :: Ord a => Set a
setFull = undefined

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = undefined

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection = undefined

setComplement :: Ord a => Set a -> Set a
setComplement = undefined

setMember :: Ord a => a -> Set a -> Bool
setMember = undefined

