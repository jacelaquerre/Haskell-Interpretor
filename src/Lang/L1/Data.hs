module Lang.L1.Data where

import Data.Map (Map)

data Expr =
    IntE Integer
  | PlusE Expr Expr
  | TimesE Expr Expr
  | BoolE Bool
  -- NEW
  -- e ∈ exp ⩴ … | (e,e)
  | PairE (Expr, Expr)
  -- e ∈ exp ⩴ … | left e | right e
  | TUnionE Expr Expr
  deriving (Eq,Ord,Show)

---------------
-- SEMANTICS --
---------------

data Value =
  IntV Integer
  | BoolV Bool
  -- NEW
  -- v ∈ value ⩴ … | (v,v)
  | PairV (Value, Value)
  -- v ∈ value ⩴ … | left v | right v
  | TUnionV Value Value
  deriving (Eq,Ord,Show)

type Env = Map String Value

data Answer =
  ValueA Value
  | BadA
  deriving (Eq,Ord,Show)
