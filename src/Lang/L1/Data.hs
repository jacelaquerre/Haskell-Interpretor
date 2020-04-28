module Lang.L1.Data where

import Data.Map (Map)

data Expr =
    IntE Integer
  | PlusE Expr Expr
  | TimesE Expr Expr
  | BoolE Bool
  | VarE String
  | LetE String Expr Expr
  -- NEW
  -- e ∈ exp ⩴ … | (e,e)
  | PairE Expr Expr
  -- e ∈ exp ⩴ … | left e | right e
  | TUnionLE String Expr
  | TUnionRE String Expr
  -- e ∈ exp ⩴ … | case e {left x ⇒ e} {right x ⇒ e}
  | CaseE Expr String Expr String Expr
  deriving (Eq,Ord,Show)

---------------
-- SEMANTICS --
---------------

data Value =
  IntV Integer
  | BoolV Bool
  -- NEW
  -- v ∈ value ⩴ … | (v,v)
  | PairV Value Value
  -- v ∈ value ⩴ … | left v | right v
  | TUnionLV String Value
  | TUnionRV String Value
  deriving (Eq,Ord,Show)

type Env = Map String Value

data Answer =
  ValueA Value
  -- | PairA (Answer, Answer)
  | BadA
  deriving (Eq,Ord,Show)
