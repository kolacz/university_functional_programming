{- Zad. 1 -}

data BTree a = Leaf | Node (BTree a) a (BTree a) deriving (Show)

t_1 :: BTree Char
t_1 = Node (Node (Node Leaf 'a' Leaf) 'b' Leaf) 'c' (Node Leaf 'd' Leaf)

dfnum :: BTree a -> BTree Integer
dfnum tree = snd(dfnum_aux tree 1)
  where dfnum_aux  Leaf        label = (label , Leaf)
        dfnum_aux (Node l _ r) label = (label2, Node t_l label t_r)
          where (label1, t_l) = dfnum_aux l (label + 1)
                (label2, t_r) = dfnum_aux r  label1

{-
bfnum :: BTree a -> BTree Integer
bfnum tree = bfnum_aux [tree] 1
  where
    bfnum_aux [] _  = (_, Leaf)
    bfnum_aux (Leaf:ys) label = (, )
    bfnum_aux ((Node l _ y):ys) label = (, )
      where -}
      

{- Zad. 2 -}

type Array a = (BTree a, Integer)

aempty :: Array a
aempty = (Leaf, 0)

asub :: Array a -> Integer -> a
asub (tree, size) n = if n > size then undefined
                      else asub_aux tree n
  where asub_aux (Node _ x _) 1 = x
        asub_aux (Node l _ r) n = if n `mod` 2 == 0
                                  then asub_aux l m
                                  else asub_aux r m
          where m = n `div` 2 

aupdate :: Array a -> Integer -> a -> Array a
aupdate arr@(tree, size) n x = if n > size then arr
                               else (aupdate_aux tree n x, size)
  where aupdate_aux (Node l _ r) 1 x = Node l x r
        aupdate_aux (Node l y r) n x = if n `mod` 2 == 0
                                       then Node (aupdate_aux l m x) y r
                                       else Node l y (aupdate_aux r m x)
          where m = n `div` 2 

ahiext :: Array a -> a -> Array a
ahiext (tree, size) x = (ahiext_aux tree (size + 1) x, size + 1)
  where ahiext_aux  Leaf        _ x = Node Leaf x Leaf
        ahiext_aux (Node _ y r) 2 x = Node (Node Leaf x Leaf) y r
        ahiext_aux (Node l y _) 3 x = Node l y (Node Leaf x Leaf)
        ahiext_aux (Node l y r) n x = if n `mod` 2 == 0
                                      then Node (ahiext_aux l m x) y r
                                      else Node l y (ahiext_aux r m x)
          where m = n `div` 2 

ahirem :: Array a -> Array a
ahirem (tree, size) = if size > 0 then (ahirem_aux tree size, size - 1)
                      else aempty
  where ahirem_aux (Node _ y r) 2 = Node Leaf y r
        ahirem_aux (Node l y _) 3 = Node l y Leaf
        ahirem_aux (Node l y r) n = if n `mod` 2 == 0
                                    then Node (ahirem_aux l m) y r
                                    else Node l y (ahirem_aux r m)
          where m = n `div` 2 

{- Zad. 3 -}

lit :: String -> (String -> a) -> String -> a
lit s = \k -> \t -> k (t ++ s)  

eol :: (String -> a) -> String -> a
eol = \k -> \s -> k (s ++ "\n")

int :: (String -> a) -> String -> (Integer -> a)
int = \k -> \s -> \n -> k (s ++ (show n))

flt :: (String -> a) -> String -> (Float -> a)
flt = \k -> \s -> \x -> k (s ++ (show x))

str :: (String -> a) -> String -> (String -> a)
str = \k -> \s -> \t -> k (s ++ t)

(^^^) d1 d2 = \k -> d1 (d2 k)

sprintf p = p (\s -> s) ""
