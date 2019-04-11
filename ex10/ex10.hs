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

{- Okasaki -} 
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

sprintf f = f (\s -> s) ""

foo = \n -> sprintf (lit "Ala ma " ^^^ int ^^^ lit " kot" ^^^ str ^^^ lit ".") n (if n == 1 then "a" else if 1 < n && n < 5 then "y" else "ow")

{- Zad. 4 -}

data Color = Red | Black deriving (Show)
data RBTree a = RBNode Color (RBTree a) a (RBTree a) | RBLeaf deriving (Show)

rbnode :: Color -> RBTree a -> a -> RBTree a -> RBTree a
rbnode Black (RBNode Red (RBNode Red a x b) y c) z d = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbnode Black a x (RBNode Red (RBNode Red b y c) z d) = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbnode Black a x (RBNode Red b y (RBNode Red c z d)) = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbnode Black (RBNode Red a x (RBNode Red b y c)) z d = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)
rbnode c a x b = RBNode c a x b

rbinsert :: Ord a => a -> RBTree a -> RBTree a
rbinsert x t = RBNode Black a y b where
  rbinsert_aux RBLeaf = RBNode Red RBLeaf x RBLeaf
  rbinsert_aux t@(RBNode c a y b) = if x < y then rbnode c (rbinsert_aux a) y b else
                                    if y < x then rbnode c a y (rbinsert_aux b) else t
  RBNode _ a y b = rbinsert_aux t
                                

rbt_1 :: RBTree Integer
rbt_1 = RBNode Black (RBNode Red (RBNode Black RBLeaf 1 RBLeaf) 3 (RBNode Black RBLeaf 4 RBLeaf)) 5 (RBNode Black RBLeaf 7 (RBNode Red RBLeaf 10 RBLeaf))

{-
       B5
    R3    B7
 B1   B4     R10

> insert 8 >

        B5
   R3         R8
 B1  B4     B7   B10


-}


{- Zad. 5 -}

rev :: [a] -> [a]
rev xs = rev_aux xs [] where
  rev_aux [] acc = acc
  rev_aux (y:ys) acc = rev_aux ys (y:acc)

halve :: [a] -> ([a], a, [a])
halve xs = halve_aux xs xs []
  where halve_aux (y:ys) []  acc = (rev acc, y, ys) 
        halve_aux (y:ys) [_] acc = (rev acc, y, ys)
        halve_aux (y:ys) (z1:z2:zs) acc = halve_aux ys zs (y:acc)

rbtreeFromList :: [a] -> RBTree a
rbtreeFromList xs = rb_aux xs
      where
        rb_aux []  = RBLeaf
        rb_aux [x] = RBNode Red RBLeaf x RBLeaf
        rb_aux  xs = RBNode Black (rb_aux h1) v (rb_aux h2)
          where (h1, v, h2) = halve xs

{- level info -} 
{-

rbtreeFromList :: [a] -> RBTree a
rbtreeFromList xs = RBNode Black a x b
  where
    RBNode _ a x b = rb_aux xs Black
      where
        rb_aux [] c = RBLeaf
        rb_aux xs c = rbnode c (rb_aux h1 (next c)) v (rb_aux h2 (next c))
          where (h1, v, h2) = halve xs
                next Black  = Red
                next  Red   = Black

-}
