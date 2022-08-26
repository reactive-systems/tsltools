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
  ) where

-------------------------------------------------------------------------------
import Data.List(inits)

import Control.Exception(assert)

import Control.Monad.Trans.Except

import TSL.Error(Error, errSolver)

import TSL.SymbolTable(Kind(..))

import TSL.Specification(Specification(..))

import TSL.Ast(AstInfo(..), SymbolInfo(..))

import TSL.ModuloTheories.Cfg(Cfg(..))

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
data DTO = DTO 
    {   theory        :: Theory
    ,   preCondition  :: TheoryPredicate
    ,   postCondition :: TheoryPredicate
    }

buildDTO :: TheoryPredicate -> TheoryPredicate -> DTO
buildDTO pre post = DTO theory pre post
  where theory   = assert theoryEq $ predTheory pre
        theoryEq = (predTheory pre) == (predTheory post)

preCond2Sygus :: TheoryPredicate -> String
preCond2Sygus = assertSmt . pred2Smt
  where assertSmt smt = "(assert " ++ smt ++ ")"

postCond2Sygus :: TheoryPredicate -> String
postCond2Sygus = undefined

-- | Gets all signals that SyGuS may need to update
-- to obtain a realizable underapproximation to TSL.
-- Intuitively, these are the cell & output signals in the post-condition.
-- Currently, it is approximated as all the signals in post-condition.
getSygusTargets :: DTO -> [TheorySymbol]
getSygusTargets (DTO _ _ post) = map symbol $ varInfos $ predInfo post

-- | Picks one target to synthesize SyGuS for.
-- Unfortunately, the current procedure only synthesizes
-- only one function for a single DTO.
-- Unfortunately, the current procedure
-- More information here:
-- tsltools/docs/tslmt2tsl-limitations.md#simultaneous-updates
pickTarget :: [TheorySymbol] -> TheorySymbol
pickTarget = head

cfg2Sygus :: TheorySymbol -> Cfg -> String
cfg2Sygus = undefined

fixedSizeQuery :: DTO -> Cfg -> String
fixedSizeQuery dto@(DTO theory pre post) cfg = undefined
  -- unlines [declTheory, toSynthesize, preCond, postCond, checkSynth]
  -- where
  --   toSynthesize = cfg2Sygus cfg $ pickTarget $ getSygusTargets dto
  --   preCond      = preCond2Sygus  pre
  --   postCond     = postCond2Sygus post
  --   declTheory   = "(set-logic " ++ show theory ++ ")"
  --   checkSynth   = "(check-synth)"

recursiveQuery :: DTO -> Cfg -> String
recursiveQuery = undefined

findRecursion :: [TAst] -> TAst
findRecursion [] = error "Empty list for recursion!"
findRecursion _  = undefined

tast2UpdateChain :: TAst -> String
tast2UpdateChain = undefined
-- (zipWith strConcat nextChains) . tastByDepth
-- where nextChains      = inits $ repeat $ show $ Next 1
--       strConcat s1 s2 = s1 ++ " " ++ s2

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
    updateTerm  = if (temporal == Eventually)
                     then updateChain ++ " W " ++ pred2Tsl post
                     else updateChain

-- | A SyGuS Query is based off of:
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusAssumptions
  :: (Int -> String -> ExceptT Error IO (Maybe TAst))
  -> [(TheoryPredicate, Temporal)]
  -> Cfg
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
