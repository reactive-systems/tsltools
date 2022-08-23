-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Generates SyGuS problems from a Data Transformation Obligation.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus
  ( DTO
  , buildDTO
  , sygusQuery
  ) where

-------------------------------------------------------------------------------
import Data.List(inits)

import Control.Exception(assert)

import TSL.Ast(AstInfo(..), SymbolInfo(..))

import TSL.ModuloTheories.CFG(CFG(..))

import TSL.ModuloTheories.Predicates( TheoryPredicate
                                    , predInfo
                                    , pred2Tsl
                                    , pred2Smt
                                    , predTheory
                                    )

import TSL.ModuloTheories.Theories( Theory
                                  , TheorySymbol
                                  , TAst
                                  , tastByDepth
                                  , tast2Tsl
                                  , symbol2Smt
                                  , symbolType
                                  , smtSortDecl
                                  , isUninterpreted
                                  )

-------------------------------------------------------------------------------

data Temporal =
      Next Int
    | Eventually
    deriving (Eq)

instance Show Temporal where
  show = \case
    Next numNext -> replicate numNext 'X'
    Eventually   -> "F"

-- | Data Transformation Obligation.
data DTO a = DTO 
    {   theory        :: Theory
    ,   preCondition  :: TheoryPredicate
    ,   postCondition :: TheoryPredicate
    }

buildDTO :: TheoryPredicate -> TheoryPredicate -> DTO
buildDTO pre post = DTO theory pre post
  where theory   = assert theoryEq $ predTheory pre
        theoryEq = predTheory pred == predTheory post

preCondition2Sygus :: TheoryPredicate -> String
preCondition2Sygus = assertSmt . pred2Smt
  where assertSmt smt = "(assert " ++ smt ++ ")"

postCondition2Sygus :: TheoryPredicate -> String
postCondition2Sygus = undefined

-- | Gets all signals that SyGuS may need to update
-- to obtain a realizable underapproximation to TSL.
-- Intuitively, these are the cell & output signals in the post-condition.
-- Currently, it is approximated as all the signals in post-condition.
getSygusTargets :: DTO -> [TheorySymbol]
getSygusTargets (DTO _ _ post) = map symbol $ varInfos $ predInfo post

cfg2Sygus :: CFG -> String
cfg2Sygus = undefined

-- TODO
-- | Builds a SyGuS query from a
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusQuery :: Temporal -> DTO -> CFG -> String
sygusQuery temporal dto@(DTO theory pre post) (CFG g _) =
  unlines [declTheory, checkSynth]
  where
    sygusTargets = getSygusTargets dto
    declTheory   = "(set-logic " ++ show theory ++ ")"
    checkSynth   = "(check-synth)"

tast2UpdateChain :: [TAst] -> String
tast2UpdateChain = (zipWith strConcat nextChains) . tastByDepth
  where nextChains      = inits $ repeat $ show $ Next 1
        strConcat s1 s2 = s1 ++ " " ++ s2

sygus2TslAss :: Temporal -> DTO -> TAst -> String
sygus2TslAss temporal (DTO _ pre post) tast = unwords
  [ "G("
  , "("
  , "(" ++ pred2Tsl pre ++ ")"
  , "&&"
  , "(" ++ updateTerm ++ ")"
  , ")"
  , "->"
  , show temporal
  , "(" ++ pred2Tsl post ++ ")"
  , ";"
  ]
  where
    updateChain = tast2UpdateChain tast
    updateTerm  = if (temporal == Next _)
                     then updateChain
                     else updateChain ++ " W " ++ pred2Tsl post

sygusAssumptions
  :: (Int -> String -> ExceptT Error (IO (Maybe TAst)))
  -> [(TheoryPredicate, Temporal)]
  -> CFG
  -> ExceptT Error IO [String]
sygusAssumptions sygusSolver preds cfg = undefined

-- (set-logic LIA)
-- (declare-const vruntime1 Int)
-- (synth-fun function ((vruntime1 Int)) Int
-- 	((I Int))(
-- 		(I Int (
-- 				(+ vruntime1 1)
-- 				(+ I 1)
-- 			)
-- 		)
-- 	)
-- )
-- (assert (= vruntime1 (- 1)))
-- (constraint (forall ((vruntime1 Int))
-- 	(=> (<= vruntime1 4)
-- 	(> (function vruntime1) vruntime1)
-- )))
-- (check-synth)
