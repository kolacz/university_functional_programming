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
ahiext = undefined 

ahirem :: Array a -> Array a
ahirem = undefined 


