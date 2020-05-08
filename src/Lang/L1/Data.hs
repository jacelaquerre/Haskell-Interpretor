module Lang.L1.Data where

import Data.Map (Map)

data Expr =
    IntE Integer
  | BoolE Bool
  | PlusE Expr Expr
  | TimesE Expr Expr
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  -- NEW
  -- e ∈ exp ⩴ … | (e,e)
  | PairE Expr Expr
  | FstE Expr
  | SndE Expr
  -- e ∈ exp ⩴ … | left e | right e
  | LeftE (Maybe Type) Expr
  | RightE (Maybe Type) Expr
  -- e ∈ exp ⩴ … | case e {left x ⇒ e} {right x ⇒ e}
  | CaseE Expr String Expr String Expr
  deriving (Eq,Ord,Show)

data Type =
    IntT
  | BoolT
  | StringT
  | PairT Type Type
  | TUnionT Type Type
  | BadT
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
  | LeftV Value
  | RightV Value
  deriving (Eq,Ord,Show)

type Env = Map String Value

data Answer =
  ValueA Value
  | BadA
  deriving (Eq,Ord,Show)
