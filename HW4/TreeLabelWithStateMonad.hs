module TreeLabelWithStateMonad where

import Store
import Control.Monad.State

-- label element

labelValue :: Ord a => a -> State (Store a Int) Int
labelValue val = do
    ls <- get
    case (lookupStore val ls) of 
        Just x -> return x
        Nothing -> do
            put (insertStore val v ls)
            return v
            where v = createNewLabel ls

-- label tree

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> State (Store a Int) (Tree Int)
labelTree Nil = do
  return Nil
labelTree (Node val left right) = do
  labeledValue <- labelValue val
  labeledLeft  <- labelTree left
  labeledRight <- labelTree right
  return (Node labeledValue labeledLeft labeledRight)

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ runState (labelTree tree) (emptyStore)
