-- Final Project
-- CS225
-- Jace Laquerre & Chris McCabe

module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing
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
  -- ⟦(e₁,e₂)⟧(γ) ≜ (v₁,v₂)           -- ⟦fst e⟧(γ) ≜ v₁
  --where                             -- where (v₁,v₂) = ⟦e⟧(γ)
  --  v₁ = ⟦e₁⟧(γ)                    -- ⟦snd e⟧(γ) ≜ v₂
  --  v₂ = ⟦e₂⟧(γ)                    -- where (v₁,v₂) = ⟦e⟧(γ)
  L1.PairE (e1, e2) -> error "TODO"
  -- ⟦left e⟧(γ) ≜ left v       -- ⟦case e₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₂⟧(γ[x₁↦v])
  -- where                      -- where left v = ⟦e₁⟧(γ)
  --  v = ⟦e⟧(γ)                -- ⟦case e₁ {left x₁ ⇒ e₂} {right x₂ ⇒ e₃}⟧(γ) ≜ ⟦e₃⟧(γ[x₂↦v])
                                -- where right v = ⟦e₁⟧(γ)
  L1.TUnionE le re -> error "TODO"


testE1 :: Test
testE1 = TestDir
  ( "E1"
  , "interp"
  , uncurry interp
  , "tests/fp/e1"
  , parseTest (pPar L1.pEnv L1.pExpr) L1.pAnswer
  )

main :: IO ()
main = runTests []
