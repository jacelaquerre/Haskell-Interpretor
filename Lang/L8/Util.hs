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

module Lang.L.Util where

import UVMHS

import Util.Lex

import Lang.L.Data

import qualified Prelude as HS
import qualified Language.Haskell.TH.Syntax as QQ
import qualified Language.Haskell.TH.Quote as QQ

import qualified Data.Map as Map

makePrettySum ''Expr
makePrettySum ''Value
makePrettySum ''IntegerHat
makePrettySum ''ValueHat
makePrettySum ''AnswerHat

deriving instance QQ.Lift Expr

level_LET        = 1
level_ASSIGN     = 2
level_PLUS       = 11
level_TIMES      = 12
level_APP        = 21
level_UNBOX      = 22
level_ACCESS     = 23

pExpr ∷ CParser TokenBasic Expr
pExpr = cpNewContext "expression" $ mixfix $ concat
  [ mixTerminal $ do cpSyntax "(" ; e ← pExpr ; cpSyntax ")" ; return e
  , mixTerminal $ do i ← cpInteger ; return $ IntE i
  , mixInfixL (𝕟64 level_PLUS) $ do cpSyntax "+" ; return PlusE
  , mixTerminal $ do b ← pBool ; return $ BoolE b
  , mixPrefix (𝕟64 level_LET) $ do
      cpSyntax "if"
      e₁ ← pExpr
      cpSyntax "then"
      e₂ ← pExpr
      cpSyntax "else"
      return $ IfE e₁ e₂
  , mixTerminal $ do x ← pVar ; return $ VarE x
  , mixPrefix (𝕟64 level_LET) $ do
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
pAnswer = pMaybe pValue

pEnv ∷ CParser TokenBasic Env
pEnv = pMap pVar pValue

pBoolHat ∷ CParser TokenBasic BoolHat
pBoolHat = cpNewContext "boolHat" $ pSet pBool

pIntHat ∷ CParser TokenBasic IntegerHat
pIntHat = cpNewContext "integerHat" $ concat
  [ do cpSyntax "_|_" ; return BotIH
  , do cpSyntax "["
       i₁ ← cpInteger
       cpSyntax ","
       i₂ ← cpInteger
       cpSyntax "]"
       return $ RangeIH i₁ i₂
  ]

pValueHat ∷ CParser TokenBasic ValueHat
pValueHat = cpNewContext "valueHat" $ do
  cpSyntax "<"
  î ← pIntHat
  cpSyntax ","
  b̂ ← pBoolHat
  cpSyntax ">"
  return $ ValueHat î b̂

pEnvHat ∷ CParser TokenBasic EnvHat
pEnvHat = pMap pVar pValueHat

pAnswerHat ∷ CParser TokenBasic AnswerHat
pAnswerHat = cpNewContext "answerHat" $ do
  cpSyntax "<"
  b ← pBool
  cpSyntax ","
  î ← pIntHat
  cpSyntax ","
  b̂ ← pBoolHat
  cpSyntax ">"
  return $ AnswerHat b $ ValueHat î b̂

parseExpr ∷ 𝕊 → IO Expr
parseExpr = parseIO pExpr *∘ tokenizeIO lexer ∘ tokens

quoteExpr ∷ HS.String → QQ.Q QQ.Exp
quoteExpr cs = do
  e ← QQ.runIO $ parseExpr $ string cs
  [| e |]

l8 ∷ QQ.QuasiQuoter
l8 = QQ.QuasiQuoter quoteExpr (const $ HS.fail $ chars "quote pattern - I can't even")
                              (const $ HS.fail $ chars "quote type - I can't even")
                              (const $ HS.fail $ chars "quote dec - I can't even")
