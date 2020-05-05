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
    L1.ValueA (L1.LeftV v) -> L1.ValueA (L1.LeftV v)
    _ -> L1.BadA
  -- ⟦left e⟧(γ) ≜ left v
  --  where
  --   v = ⟦e⟧(γ)
  L1.RightE _ e -> case interp env e of
    L1.ValueA (L1.RightV v) -> L1.ValueA (L1.RightV v)
    _ -> L1.BadA
  -- ⟦case e₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₂⟧(γ[x₁↦v])
    -- where left v = ⟦e₁⟧(γ)
  -- ⟦case e₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₃⟧(γ[x₂↦v])
    -- where right v = ⟦e₁⟧(γ)
  L1.CaseE e1 x1 e2 x2 e3 -> error "TODO"

test1 :: Test
test1 = TestDir
  ( "T1"                 -- e.g., "T1"
  , "identity function"  -- e.g., "interp"
  , id                   -- the function, e.g., (\ e -> interp e Map.empty Map.empty)
  , "tests/fp/interp"           -- the directory where tests live, e.g., "tests/fp/t1"
  , parseTest L1.pExpr L1.pExpr
  )

typeChecker :: L1.Expr -> L1.Env -> Maybe(L1.Type)
typeChecker e env = case e of
  -- ⦇i⦈(Γ) ≜ int
  L1.IntE i -> Just L1.IntT
  -- ⦇b⦈(Γ) ≜ bool
  L1.BoolE b -> Just L1.BoolT
  -- ⦇e₁ + e₂⦈(Γ) ≜ int
    -- where int = ⦇e₁⦈(Γ)
          -- int = ⦇e₂⦈(Γ)
  L1.PlusE e1 e2 -> case (typeChecker e1 env, typeChecker e2 env) of
    Just (L1.IntT) -> Just (L1.IntT)
  L1.TimesE e1 e2 -> error "TODO"
  -- ⦇if e₁ then e₂ else e₃⦈(Γ) ≜ τ
  -- where bool = ⦇e₁⦈(Γ)
        -- τ    = ⦇e₂⦈(Γ)
        -- τ    = ⦇e₃⦈(Γ)
  L1.IfE e1 e2 e3 -> error "TODO"
  -- ⦇x⦈(Γ) ≜ τ
  L1.VarE x -> error "TODO"
   -- where τ = Γ(x)
   --L1.Type e -> error "TODO"
  -- ⦇let x = e₁ in e₂⦈(Γ) ≜ τ₂
  -- where τ₁ = ⦇e₁⦈(Γ)
        -- τ₂ = ⦇e₂⦈(Γ[x↦τ₁])
  L1.LetE x e1 e2 -> error "TODO"
  L1.PairE e1 e2 -> error "TODO"
  L1.LeftE t e -> error "TODO"
  L1.RightE t e -> error "TODO"
  L1.FstE e -> error "TODO"
  L1.SndE e -> error "TODO"
  L1.CaseE e1 x1 e2 x2 e3 -> error "TODO"

test2 :: Test
test2 = TestDir
  ( "T1"
  , "identity function"
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
  --putStrLn "EXAMPLE"
  --putStrLn (show [lme| let p = (1,2) in fst p |])
