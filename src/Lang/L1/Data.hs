module Lang.L1.Data where

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  deriving (Eq,Ord,Show)

data Value = 
  IntV Integer
  deriving (Eq,Ord,Show)

data Answer = 
  ValueA Value
  deriving (Eq,Ord,Show)

