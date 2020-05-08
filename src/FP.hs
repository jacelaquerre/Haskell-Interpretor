-- Final Project
-- CS225
-- Jace Laquerre & Chris McCabe

module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing
import Util.Lex
import qualified Lang.L1 as L1


interp :: L1.Env -> L1.Expr -> L1.Answer
interp env e = case e of
  L1.IntE i -> L1.ValueA (L1.IntV i)
  L1.PlusE e1 e2 -> case (interp env e1, interp env e2) of
    (L1.ValueA (L1.IntV i1), L1.ValueA (L1.IntV i2)) -> L1.ValueA (L1.IntV (i1 + i2))
    _ -> L1.BadA
  L1.TimesE e1 e2 -> case (interp env e1, interp env e2) of
    (L1.ValueA (L1.IntV i1), L1.ValueA (L1.IntV i2)) -> L1.ValueA (L1.IntV (i1 * i2))
    _ -> L1.BadA
  L1.BoolE b -> L1.ValueA (L1.BoolV b)
  L1.IfE e1 e2 e3 -> case interp env e1 of
    L1.ValueA (L1.BoolV b) ->
      if b
         then interp env e2
         else interp env e3
    _ -> L1.BadA
  L1.VarE x -> case Map.lookup x env of
    Just v -> L1.ValueA v
    Nothing -> L1.BadA
  L1.LetE x e1 e2 -> case interp env e1 of
    L1.ValueA v -> interp (Map.insert x v env) e2
    _ -> L1.BadA
  -- ⟦(e₁,e₂)⟧(γ) ≜ (v₁,v₂)
  --where
  --  v₁ = ⟦e₁⟧(γ)
  --  v₂ = ⟦e₂⟧(γ)
  L1.PairE e1 e2 -> case interp env e1 of
    L1.ValueA v1 -> case interp env e2 of
        L1.ValueA v2 -> L1.ValueA (L1.PairV v1 v2)
        _ -> L1.BadA
    _  -> L1.BadA
  -- ⟦fst e⟧(γ) ≜ v₁
  -- where (v₁,v₂) = ⟦e⟧(γ)
  L1.FstE e -> case interp env e of
    L1.ValueA (L1.PairV v1 _) -> L1.ValueA (v1)
    _ -> L1.BadA
  -- ⟦snd e⟧(γ) ≜
  -- where (v₁,v₂) = ⟦e⟧(γ)
  L1.SndE e -> case interp env e of
    L1.ValueA (L1.PairV _ v2) -> L1.ValueA (v2)
    _ -> L1.BadA
  -- ⟦left e⟧(γ) ≜ left v
  -- where
  --  v = ⟦e⟧(γ)
  L1.LeftE _ e -> case interp env e of
    L1.ValueA (L1.LeftV vL) -> L1.ValueA (L1.LeftV vL)
    _ -> L1.BadA
  -- ⟦left e⟧(γ) ≜ left v
  --  where
  --   v = ⟦e⟧(γ)
  L1.RightE _ e -> case interp env e of
    L1.ValueA (L1.RightV vR) -> L1.ValueA (L1.RightV vR)
    _ -> L1.BadA
  -- ⟦casee₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₂⟧(γ[x₁↦v])
    -- where left v = ⟦e₁⟧(γ)
  -- ⟦case e₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₃⟧(γ[x₂↦v])
    -- where right v = ⟦e₁⟧(γ)
  L1.CaseE e1 x1 e2 x2 e3 -> case interp env e1 of
    L1.ValueA (L1.LeftV v1) -> interp env e2
    L1.ValueA (L1.RightV v2) -> interp env e3
    _ -> L1.BadA

test1 :: Test
test1 = TestDir
  ( "T1"                 -- e.g., "T1"
  , "interp"             -- e.g., "interp"
  , id                   -- the function, e.g., (\ e -> interp e Map.empty Map.empty)
  , "tests/fp/interp"    -- the directory where tests live, e.g., "tests/fp/t1"
  , parseTest L1.pExpr L1.pExpr
  )

typeChecker :: L1.Expr -> L1.Env -> L1.Type
typeChecker e env = case e of
  -- ⦇i⦈(Γ) ≜ int
  L1.IntE _i -> L1.IntT
  -- ⦇b⦈(Γ) ≜ bool
  L1.BoolE _b -> L1.BoolT
  -- ⦇e₁ + e₂⦈(Γ) ≜ int
    -- where int = ⦇e₁⦈(Γ)
          -- int = ⦇e₂⦈(Γ)
  L1.PlusE e1 e2 -> case (typeChecker e1 env) of
    t1  -> case (typeChecker e2 env) of
      t2 -> t2
  L1.TimesE e1 e2 -> case (typeChecker e1 env) of
    t1 -> case (typeChecker e2 env) of
      t2 -> t2
  -- ⦇if e₁ then e₂ else e₃⦈(Γ) ≜ τ
  -- where bool = ⦇e₁⦈(Γ)
        -- τ    = ⦇e₂⦈(Γ)
        -- τ    = ⦇e₃⦈(Γ)
  L1.IfE e1 e2 e3 -> case (interp env e1) of
    L1.ValueA (L1.BoolV b) ->
      if b
        then (typeChecker e2 env)
        else (typeChecker e3 env)
    _ -> L1.BadT
  -- ⦇x⦈(Γ) ≜ τ
  L1.VarE _x -> L1.StringT
   -- where τ = Γ(x)
  -- ⦇let x = e₁ in e₂⦈(Γ) ≜ τ₂
  -- where τ₁ = ⦇e₁⦈(Γ)
        -- τ₂ = ⦇e₂⦈(Γ[x↦τ₁])
  L1.LetE x e1 e2 -> case (typeChecker e1 env) of
    t1 -> case (typeChecker e2 env) of
      t2 -> t2
  L1.PairE e1 e2 -> case (typeChecker e1 env) of
    t1 -> case (typeChecker e2 env) of
      t2 -> L1.PairT t1 t2
  L1.LeftE t e -> L1.TUnionT undefined (typeChecker e env)
  L1.RightE t e -> L1.TUnionT undefined (typeChecker e env)
  L1.FstE e -> (typeChecker e env)
  L1.SndE e -> (typeChecker e env)
  L1.CaseE e1 x1 e2 x2 e3 -> undefined

test2 :: Test
test2 = TestDir
  ( "T1"
  , "typeChecker"
  , id
  , "tests/fp/typeChecker"
  , parseTest L1.pExpr L1.pExpr
  )

main :: IO ()
main = do
  putStrLn "TESTS"
  runTests
    [ test1,
      test2
    ]
