{-# LANGUAGE 
    ConstraintKinds
   ,DataKinds
   ,ExplicitNamespaces
   ,FlexibleContexts
   ,FlexibleInstances
   ,FunctionalDependencies
   ,GADTs
   ,GeneralizedNewtypeDeriving
   ,InstanceSigs
   ,KindSignatures
   ,LambdaCase
   ,MonadComprehensions
   ,MultiParamTypeClasses
   ,NoImplicitPrelude
   ,OverloadedStrings
   ,PartialTypeSignatures
   ,PatternSynonyms
   ,PolyKinds
   ,QuantifiedConstraints
   ,RankNTypes
   ,RebindableSyntax
   ,ScopedTypeVariables
   ,StandaloneDeriving
   ,Strict
   ,StrictData
   ,TemplateHaskell
   ,TypeApplications
   ,TypeFamilies
   ,TypeOperators
   ,UndecidableInstances
   ,UndecidableSuperClasses
   ,UnicodeSyntax
   ,ViewPatterns 
   ,DeriveLift #-}

module Util.Lex where

import UVMHS

import qualified Prelude as HS

import qualified Data.Set as Set
import qualified Data.Map as Map

instance (Ord a,Pretty a) ⇒ Pretty (Set.Set a) where pretty = pretty ∘ pow ∘ Set.toList
instance (Ord k,Pretty k,Pretty v) ⇒ Pretty (Map.Map k v) where pretty = pretty ∘ assoc ∘ frhs ∘ Map.toList
instance (Pretty a) ⇒ Pretty (HS.Maybe a) where pretty = pretty ∘ \case { HS.Nothing → None ; HS.Just x → Some x}

lexer ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexer = lexerBasic puns kws prim ops
  where
    puns = list ["(",")","{","}","[","]","<",">",".",",",";",":","=","->","=>","<-","<=","!","#"]
    kws = list 
      [ "TEST","EXPECTED","AND"
      , "let","in","if","then","else"
      , "object"
      , "def"
      , "do"
      , "nothing"
      , "fun"
      , "box"
      , "class","fields","method","end","new","object"
      ]
    prim = list ["true","false","bad","loc","_|_"]
    ops = list ["+","-","*","/","<?",">?","<=?",">=?","=?","/=?","||","&&"]

pBool ∷ CParser TokenBasic 𝔹
pBool = concat
  [ do cpSyntax "true" ; return True
  , do cpSyntax "false" ; return False
  ]

pInt ∷ CParser TokenBasic ℤ
pInt = cpInteger

pString ∷ CParser TokenBasic [ℂ]
pString = chars ^$ cpString

pVar ∷ CParser TokenBasic [ℂ]
pVar = chars ∘ 𝕩name ^$ cpName

pSet ∷ (Ord a) ⇒ CParser TokenBasic a → CParser TokenBasic (Set.Set a)
pSet pX = cpNewContext "set" $ do
  cpSyntax "{"
  xs ← cpManySepBy (cpSyntax ",") pX
  cpSyntax "}"
  return $ Set.fromList $ lazyList xs

pList ∷ CParser TokenBasic a → CParser TokenBasic [a]
pList pX = cpNewContext "list" $ do
  cpSyntax "["
  xs ← cpManySepBy (cpSyntax ",") pX
  cpSyntax "]"
  return $ tohs xs

pMap ∷ (Ord k) ⇒ CParser TokenBasic k → CParser TokenBasic v → CParser TokenBasic (Map.Map k v)
pMap pK pV = cpNewContext "map" $ do
  cpSyntax "{"
  xvs ← cpManySepBy (cpSyntax ",") $ do
    x ← pK
    cpSyntax "="
    v ← pV
    return (x,v)
  cpSyntax "}"
  return $ Map.fromList $ tohs xvs

pPair ∷ CParser TokenBasic a → CParser TokenBasic b → CParser TokenBasic (a,b)
pPair pX pY = cpNewContext "pair" $ do
  x ← pX
  cpSyntax "AND"
  y ← pY
  return (x,y)

pTup ∷ CParser TokenBasic a → CParser TokenBasic b → CParser TokenBasic (a,b)
pTup pX pY = cpNewContext "tup" $ do
  cpSyntax "("
  x ← pX
  cpSyntax ","
  y ← pY
  cpSyntax ")"
  return (x,y)

pMany ∷ CParser TokenBasic a → CParser TokenBasic [a]
pMany = tohs ^∘ cpMany

pMaybe ∷ CParser TokenBasic a → CParser TokenBasic (HS.Maybe a)
pMaybe xM = tries 
  [ do cpSyntax "nothing" 
       return HS.Nothing 
  , HS.Just ^$ xM 
  ]

pTest ∷ CParser TokenBasic a → CParser TokenBasic b → CParser TokenBasic (a,b)
pTest pA pB = cpNewContext "test" $ concat
  [ do cpSyntax "TEST"
       e ← pA
       cpSyntax "EXPECTED"
       a ← pB
       return (e,a)
  ]

parseTest ∷ (Pretty a,Pretty b) ⇒ CParser TokenBasic a → CParser TokenBasic b → 𝕊 → IO (a,b)
parseTest pA pB = parseIO (pTest pA pB) *∘ tokenizeIO lexer ∘ tokens
