module Lang.Trees.Data where

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq,Ord,Show)
