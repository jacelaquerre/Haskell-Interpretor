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

module Lang.L1.Util where

import UVMHS

import Util.Lex

import Lang.L1.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

import qualified Data.Map as Map

makePrettySum ''Expr
makePrettySum ''Value
makePrettySum ''Answer

deriving instance QQ.Lift Expr
deriving instance QQ.Lift Value
deriving instance QQ.Lift Answer

pExpr ∷ CParser TokenBasic Expr
pExpr = cpNewContext "expression" $ mixfix $ concat
  [ mixTerminal $ do cpSyntax "(" ; e ← pExpr ; cpSyntax ")" ; return e
  , mixTerminal $ do i ← cpInteger ; return $ IntE i
  , mixInfixL (𝕟64 2) $ do cpSyntax "+" ; return PlusE
  , mixInfixL (𝕟64 3) $ do cpSyntax "*" ; return TimesE
  , mixTerminal $ do b ← pBool ; return $ BoolE b
  , mixTerminal $ do x ← pVar ; return $ VarE x
  , mixPrefix (𝕟64 1) $ do
      cpSyntax "let"
      x ← pVar
      cpSyntax "="
      e ← pExpr
      cpSyntax "in"
      return $ LetE x e
  ]

pValue ∷ CParser TokenBasic Value
pValue = cpNewContext "value" $ concat
  [ do i ← cpInteger ; return $ IntV i
  , do b ← pBool ; return $ BoolV b
  ]

pAnswer ∷ CParser TokenBasic Answer
pAnswer = cpNewContext "answer" $ concat
  [ do v ← pValue ; return $ ValueA v
  , do cpSyntax "bad" ; return BadA
  ]

pEnv ∷ CParser TokenBasic Env
pEnv = cpNewContext "env" $ do
  cpSyntax "{"
  xvs ← cpManySepBy (cpSyntax ",") $ do
    x ← pVar
    cpSyntax "="
    v ← pValue
    return (x,v)
  cpSyntax "}"
  return $ Map.fromList $ tohs xvs

parseExpr ∷ 𝕊 → IO Expr
parseExpr = parseIO pExpr *∘ tokenizeIO lexer ∘ tokens

quoteExpr ∷ HS.String → QQ.Q QQ.Exp
quoteExpr cs = do
  e ← QQ.runIO $ parseExpr $ string cs
  [| e |]

l1 ∷ QQ.QuasiQuoter
l1 = QQ.QuasiQuoter quoteExpr (const $ HS.fail $ chars "quote pattern - I can't even")
                                (const $ HS.fail $ chars "quote type - I can't even")
                                (const $ HS.fail $ chars "quote dec - I can't even")
