module TreeLabelWithoutStateMonad where

import Store

-- label element

labelValue :: Ord a => a -> (Store a Int) -> (Int, Store a Int)
labelValue val ls = 
    let v = lookupStore val ls
        x = if v == Nothing
                then createNewLabel ls
                else extract v
        y = insertStore val x ls
    in (x, y)

extract :: Maybe Int -> Int
extract (Just a)  = a
extract (Nothing) = -1


-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> (Store a Int) -> (Tree Int, Store a Int)
labelTree Nil ls = (Nil, ls)
  
labelTree (Node val left right) ls =
  (Node labeledValue labeledLeft labeledRight, ls''')
    where (labeledValue, ls')   = labelValue val   ls
          (labeledLeft,  ls'')  = labelTree  left  ls'
          (labeledRight, ls''') = labelTree  right ls''

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ labelTree tree emptyStore
