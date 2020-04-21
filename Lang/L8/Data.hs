module Lang.L8.Data where

import Data.Map (Map)
import Data.Set (Set)

-----------------
-- EXPRESSIONS --
-----------------

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | BoolE Bool
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  deriving (Eq,Ord,Show)

------------------------
-- CONCRETE SEMANTICS --
------------------------

data Value = 
    IntV Integer
  | BoolV Bool
  deriving (Eq,Ord,Show)

type Env = Map String Value

type Answer = Maybe Value

------------------------
-- ABSTRACT SEMANTICS --
------------------------

data IntegerHat = 
    BotIH
  | RangeIH Integer Integer -- invariant (RangeIH lb ub): lb ≤ ub
  deriving (Eq,Ord,Show)

type BoolHat = Set Bool

data ValueHat = 
    ValueHat IntegerHat BoolHat
  deriving (Eq,Ord,Show)

type EnvHat = Map String ValueHat

data AnswerHat = 
    AnswerHat Bool ValueHat
  deriving (Eq,Ord,Show)
