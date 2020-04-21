module HW08 where

{-

Name:

Jace Laquerre & Chris McCabe

Collaboration Statement:

Partners

Collaboration on high-level ideas and approach on assignments is
encouraged. Copying someone else's work is not allowed. Copying solutions
from an online source is not allowed. Any collaboration or online
resources, even if used only a small amount, must be declared in your
assignment when you submit it. For example: “I discussed high-level
strategies for solving problem 2 and 5 with Alex; I found this
stackoverflow post (<link>) helpful while working on problem 3.” Students
caught copying work are eligible for immediate failure of the course and
disciplinary action by the University. All academic integrity misconduct
will be treated according to UVM's Code of Academic Integrity.

-}

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing

import Util.Lex

import qualified Lang.L7 as L7

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ========= --
-- UTILITIES --
-- ========= --

freshInteger :: [Integer] -> Integer
freshInteger is = case is of
  [] -> 0
  i:is' ->
    let i' = freshInteger is'
    in max (i + 1) i'

freshLocation :: L7.Store -> Integer
freshLocation sto = freshInteger (Map.keys sto)

-- ======== --
-- EXAMPLES --
-- ======== --

{-

Here is an example program in our new object-oriented language:
let Point =
      class
      fields x y
      method get-x => !self.x
      method get-y => !self.y
      method set-x => fun x => self.x <- x
      method set-y => fun y => self.y <- y
      method norm-sq => self.get-x * self.get-x + self.get-y * self.get-y
      method double =>
        self.set-x (self.get-x * 2) ;
        self.set-y (self.get-y * 2)
      end
in
let p = new Point { x = 1, y = 2 } in
let x = p.norm-sq in
p.set-x 3 ;
p.set-y 4 ;
p.double ;
x + p.norm-sq

Notice the new language constructs:
- `class` for creating a new class
- `new` for creating a new object
- `e.name` for accessing the field or method of an object

New terms:

e ∈ expr ⩴ …
         | class fields x … x [method x => e] … [method x => e] end
         | new e { x = e , … , x = e }
         | e.x

Class expressions will evaluate to class values, which close over the
environment just like lambdas, and new object expressions evalute to object
values, which also keep a copy of the closure environment created at class
creation time.

v ∈ value ⩴ …
          | ⟨class fields x … x [method x => e] … [method x => e] end, γ⟩
          | ⟨object fields {x ↦ ℓ , … , x ↦ ℓ} methods {x ↦ e , … , x ↦ e}, γ⟩

-}

-- ========== --
-- ASSIGNMENT --
-- ========== --

---------------
-- [E1]: ★☆☆ --
---------------

-- When we create new objects with object creation syntax:
--
--     new Point { x = 1, y = 2 }
--
-- We need to check that the keys in the syntax `x` and `y` correspond with the
-- same names from the class definition's syntax:
--
--     class
--     fields x y
--     …
--
-- At this point in the interpreter, `fields x y` is represented as a list of
-- strings:
--
--     ["x","y"]
--
-- and the object creation syntax is represented as:
--
--     [("x", IntE 1), ("y", IntE 2)]
--
-- We need a function that checks that the first list is the same as the first
-- elements of the second list, up to permutation. Here is the formal spec:
--
--     checkFields ∈ list(var) ∧ list(var × expr) → 𝔹
--     checkFields((x₁,…,xₙ),[(y₁,e₁),…,(yₙ,eₙ)]) ≜ {x₁,…,xₙ} = {y₁,…,yₙ}
--
-- Notice that `{x₁,…,xₙ}` and `{y₁,…,yₙ}` must be equal *as sets*; it isn't
-- enough to be the same list, because they must be the same list up to any
-- permutation. For example, if we swapped the "x" and the "y", then
-- it should still return the same result, that is,
--
--   checkFields ["y", "x"] [("x", IntE 1), ("y", IntE 2)]
--
-- should return True.
--
-- HINTS:
-- - convert each list to a Haskell Set datatype and use Haskell's equality operator (==).
-- - to convert a list to a set, use Set.fromList.
-- - to turn a list of pairs into a list of the first element of each pair, use `map fst`.

checkFields :: [String] -> [(String,L7.Expr)] -> Bool
checkFields fields fieldls =
   if (Set.fromList fields == Set.fromList (map fst  fieldls))
      then True
        else False



testE1 :: Test
testE1 = TestDir
  ( "E1"
  , "checkFields"
  , uncurry checkFields
  , "tests/hw08/e1"
  , parseTest (pPair (pList pVar) (pList (pTup pVar L7.pExpr))) pBool
  )

---------------
-- [E2]: ★★☆ --
---------------

-- To interpret object creation syntax, such as:
--
--     new Point { x = 1 + 1, y = 2 + 2 }
--
-- we need to:
-- - evaluate each of the field initialization expressions, `1 + 1` and `2 + 2`
-- - allocate new locations in the store to hold these results, because object
--   fields are mutable
-- - return a map from field names to locations, where each location is
--   initalized to the value returned by evaluating the initialization
--   expression
--
-- For example, in the `Point` example shown above, this should:
-- - evalute `1 + 1` and `2 + 2` to the values `2` and `4`
-- - allocate new locations `loc 1` and `loc 2` in the store for each result
-- - map each location to a field value in the store, so `loc 1 ↦ 2` and `loc 2 ↦ 4`
-- - return a map from field names to the locations that hold their values, so
--   `x ↦ loc 1` and `y ↦ loc 2`
--
-- The spec for the function is as follows:
--
--     interp-fields ∈ list(var × expr) × env × store → maybe((var ⇀ location) × store)
--     interp-fields([(x₁,e₁),…,(xₙ,eₙ)],γ,σ₀) ≜ ({x₁ ↦ ℓ₁,…,xₙ ↦ ℓₙ}, σ′)
--       where
--         (v₁,σ₁) = ⟦e₁⟧(γ,σ₀)
--                 ⋮
--         (vₙ,σₙ) = ⟦eₙ⟧(γ,σₙ₋₁)
--         {ℓ₁,…,ℓₙ} ∩ dom(σₙ) = ∅
--         ℓᵢ ≠ ℓⱼ for i ≠ j
--         σ′ = σₙ[ℓ₁↦v₁,…,ℓₙ↦vₙ]
--
-- HINTS:
-- - Define this function by recursion on the list of fields
-- - This definition should call `interp`, and `interp` should call this
--   definition (`interpFields`). These functions will be mutually recursive.
-- - Don't forget to "thread" the store,
-- - Don't forget to handle all pattern match cases
-- - You'll want to use `freshLocation`, provided above in the utilities
--   section.
-- - Although the specification doesn't say what order the locations are
--   allocated in, the tests expect a specific ordering. Look at the expected
--   input and output behavior from the tests in order to make sure your
--   implementation is doing the right allocation order to pass the tests.
-- - You might find helpful these functions from the Map datatype api:
--
--       Map.empty :: Map k v
--       Map.insert :: k -> v -> Map k v -> Map k v

interpFields :: [(String, L7.Expr)] -> L7.Env -> L7.Store -> Maybe (Map String Integer, L7.Store)
interpFields fields env sto = case fields of
  [(x,e)] -> case (interp e env sto) of
    Just (L7.IntV i, sto') -> Map.insert x i sto'
    --Just (L7.BoolV b, sto') -> Map.insert (freshLocation sto) b sto'
    _ -> Nothing
  _ -> Nothing

testE2 :: Test
testE2 = TestDir
  ( "E2"
  , "interpFields"
  , uncurry $ uncurry interpFields
  , "tests/hw08/e2"
  , parseTest (pPair (pPair (pList (pTup pVar L7.pExpr)) L7.pEnv) L7.pStore) $
      pMaybe (pPair (pMap pVar L7.pLoc) L7.pStore)
  )

---------------
-- [E3]: ★★★ --
---------------

-- Write an interpreter for our new core language with first class classes and
-- objects. The interpreter has the same definition from HW07 for all of the
-- syntax that isn't new in this homework. The interpreter definition for new
-- terms is as follows:
--
--     ⟦_⟧ ∈ expr → env × store → maybe(value × store)
--     ⟦class fields x₁ … xₙ [method x′₁ => e₁] … [method x′ₘ => eₘ] end⟧(γ,σ) ≜ (v,σ)
--       where
--       v = ⟨class fields x₁ … xₙ [method x′₁ => e₁] … [method x′ₘ => eₘ] end, γ⟩
--     ⟦new e { x₁ = e₁ , … , xₙ = eₙ }⟧(γ,σ) ≜ (v′,σ′)
--       where
--         (v, σ₀) = ⟦e⟧(γ,σ)
--         v = ⟨class fields y₁ … yₙ [method y′₁ => e′₁] … [method y′ₘ => e′ₘ] end, γ′⟩
--         {x₁,…,xₙ} = {y₁,…,yₙ}
--         (v₁,σ₁) = ⟦e₁⟧(γ,σ₀)
--                 ⋮
--         (vₙ,σₙ) = ⟦eₙ⟧(γ,σₙ₋₁)
--         {ℓ₁,…,ℓₙ} ∩ dom(σₙ) = ∅
--         ℓᵢ ≠ ℓⱼ for i ≠ j
--         v′ = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {y′₁ ↦ e′₁ , … , y′ₘ ↦ e′ₘ }, γ′⟩
--         σ′ = σₙ[ℓ₁↦v₁,…,ℓₙ↦vₙ]
--    ⟦e.x⟧(γ,σ) ≜ (loc ℓᵢ,σ′)
--      where
--        (v, σ′) = ⟦e⟧(γ,σ)
--        v = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {x′₁ ↦ e₁ , … , x′ₘ ↦ eₘ}, γ′⟩
--        x = xᵢ
--    ⟦e.x⟧(γ,σ) ≜ ⟦eᵢ⟧(γ′[self↦v],σ′)
--      where
--        (v, σ′) = ⟦e⟧(γ,σ)
--        v = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {x′₁ ↦ e₁ , … , x′ₘ ↦ eₘ}, γ′⟩
--        x ∉ {x₁,…,xₙ}
--        x = x′ᵢ

interp :: L7.Expr -> L7.Env -> L7.Store -> Maybe (L7.Value, L7.Store)
interp e env sto = case e of
  L7.IntE i -> Just (L7.IntV i, sto)
  L7.PlusE e1 e2 -> case interp e1 env sto of
    Just (L7.IntV i1, sto') -> case interp e2 env sto' of
      Just (L7.IntV i2, sto'') -> Just (L7.IntV (i1 + i2), sto'')
      _ -> Nothing
    _ -> Nothing
  L7.TimesE e1 e2 -> case interp e1 env sto of
    Just (L7.IntV i1, sto') -> case interp e2 env sto' of
      Just (L7.IntV i2, sto'') -> Just (L7.IntV (i1 * i2), sto'')
      _ -> Nothing
    _ -> Nothing
  L7.BoolE b -> Just (L7.BoolV b, sto)
  L7.IfE e1 e2 e3 -> case interp e1 env sto of
    Just (L7.BoolV b, sto') ->
      if b
         then interp e2 env sto'
         else interp e3 env sto'
    _ -> Nothing
  L7.VarE x -> case Map.lookup x env of
    Just v -> Just (v, sto)
    _ -> Nothing
  L7.LetE x e1 e2 -> case interp e1 env sto of
    Just (v, sto') -> interp e2 (Map.insert x v env) sto'
    _ -> Nothing
  L7.FunE x e' -> Just (L7.CloV x e' env, sto)
  L7.AppE e1 e2 -> case interp e1 env sto of
    Just (L7.CloV x e' env', sto') -> case interp e2 env sto' of
      Just (v, sto'') -> interp e' (Map.insert x v env') sto''
      _ -> Nothing
    _ -> Nothing
  L7.BoxE e' -> case interp e' env sto of
    Just (v, sto') ->
      let l = freshLocation sto'
      in Just (L7.LocV l, Map.insert l v sto')
    _ -> Nothing
  L7.UnboxE e' -> case interp e' env sto of
    Just (L7.LocV l, sto') -> case Map.lookup l sto' of
      Just v ->  Just (v, sto')
      _ -> Nothing
    _ -> Nothing
  L7.AssignE e1 e2 -> case interp e1 env sto of
    Just (L7.LocV l, sto') -> case interp e2 env sto' of
      Just (v, sto'') -> Just (v, Map.insert l v sto'')
      _ -> Nothing
    _ -> Nothing
  --     ⟦_⟧ ∈ expr → env × store → maybe(value × store)
  --     ⟦class fields x₁ … xₙ [method x′₁ => e₁] … [method x′ₘ => eₘ] end⟧(γ,σ) ≜ (v,σ)
  --       where
  --       v = ⟨class fields x₁ … xₙ [method x′₁ => e₁] … [method x′ₘ => eₘ] end, γ⟩
  --     ⟦new e { x₁ = e₁ , … , xₙ = eₙ }⟧(γ,σ) ≜ (v′,σ′)
  --       where
  --         (v, σ₀) = ⟦e⟧(γ,σ)
  --         v = ⟨class fields y₁ … yₙ [method y′₁ => e′₁] … [method y′ₘ => e′ₘ] end, γ′⟩
  --         {x₁,…,xₙ} = {y₁,…,yₙ}
  --         (v₁,σ₁) = ⟦e₁⟧(γ,σ₀)
  --                 ⋮
  --         (vₙ,σₙ) = ⟦eₙ⟧(γ,σₙ₋₁)
  --         {ℓ₁,…,ℓₙ} ∩ dom(σₙ) = ∅
  --         ℓᵢ ≠ ℓⱼ for i ≠ j
  --         v′ = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {y′₁ ↦ e′₁ , … , y′ₘ ↦ e′ₘ }, γ′⟩
  --         σ′ = σₙ[ℓ₁↦v₁,…,ℓₙ↦vₙ]

  L7.ClassE fields methods -> error "TODO"
  --    ⟦e.x⟧(γ,σ) ≜ (loc ℓᵢ,σ′)
  --      where
  --        (v, σ′) = ⟦e⟧(γ,σ)
  --        v = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {x′₁ ↦ e₁ , … , x′ₘ ↦ eₘ}, γ′⟩
  --        x = xᵢ
  --    ⟦e.x⟧(γ,σ) ≜ ⟦eᵢ⟧(γ′[self↦v],σ′)
  --      where
  L7.NewE e' fieldes -> error "TODO"
  --    ⟦e.x⟧(γ,σ) ≜ ⟦eᵢ⟧(γ′[self↦v],σ′)
  --      where
  --        (v, σ′) = ⟦e⟧(γ,σ)
  --        v = ⟨object fields {x₁ ↦ ℓ₁ , … , xₙ ↦ ℓₙ} methods {x′₁ ↦ e₁ , … , x′ₘ ↦ eₘ}, γ′⟩
  --        x ∉ {x₁,…,xₙ}
  --        x = x′ᵢ
  L7.AccessE e' name -> error "TODO"

testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interp"
  , (\ e -> case interp e Map.empty Map.empty of { Just (v,_) -> Just v ; Nothing -> Nothing })
  , "tests/hw08/e3"
  , parseTest L7.pExpr $ pMaybe L7.pValue
  )

main :: IO ()
main = runTests
  -- assignment
  [ testE1
  , testE2
  , testE3
  ]
