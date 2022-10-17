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
  ( Dto
  , buildDto
  , buildDtoList
  , fixedSizeQuery
  ) where

-------------------------------------------------------------------------------

import qualified Data.Set as Set

import Data.Set (Set)

import qualified Data.Map as Map

import Data.Map (Map)

import Data.List (inits)

import Control.Exception(assert)

import Control.Monad.Trans.Except

import TSL.Error(Error, errSolver)

import TSL.Specification(Specification(..))

import TSL.Ast(Ast(..), AstInfo(..), SymbolInfo(..))

import TSL.ModuloTheories.Cfg(Cfg(..), outputSignals, productionRules)

import TSL.ModuloTheories.Predicates( TheoryPredicate
                                    , predInfo
                                    , pred2Tsl
                                    , pred2Smt
                                    , predTheory
                                    , predSignals
                                    , predReplacedSmt
                                    , enumeratePreds
                                    )

import TSL.ModuloTheories.Theories( Theory
                                  , TheorySymbol
                                  , TAst
                                  , tastByDepth
                                  , tast2Tsl
                                  , tastSignals
                                  , tast2Smt
                                  , symbol2Smt
                                  , symbolType
                                  , symbolTheory
                                  , smtSortDecl
                                  , isUninterpreted
                                  , getAst
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
data Dto = Dto 
    {   theory        :: Theory
    ,   preCondition  :: TheoryPredicate
    ,   postCondition :: TheoryPredicate
    }

buildDto :: TheoryPredicate -> TheoryPredicate -> Dto
buildDto pre post = Dto theory pre post
  where theory   = assert theoryEq $ predTheory pre
        theoryEq = (predTheory pre) == (predTheory post)

buildDtoList :: [TheoryPredicate] -> [Dto]
buildDtoList preds = concat $ map buildWith preds
  where buildWith pred = map (buildDto pred) preds

tabulate :: Int -> String -> String
tabulate n = (++) (replicate n '\t')

minitab :: Int -> String -> String
minitab n = (++) (replicate (2 * n) ' ')

functionName :: String
functionName = "function"

parenthize :: Int -> String -> String
parenthize repeats str = lpars ++ str ++ rpars
  where lpars = replicate repeats '(' 
        rpars = replicate repeats ')'

preCond2Sygus :: TheoryPredicate -> String
preCond2Sygus = assertSmt . pred2Smt
  where assertSmt smt = parenthize 1 $ "assert " ++ smt

-- (constraint (>= (function users) 0))
-- Replace all instances of the TheorySymbol to function application
postCond2Sygus :: TheorySymbol -> TheoryPredicate -> String
postCond2Sygus signal postCond = parenthize 1 $ unwords [constraint, clause]
  where fApplied   = parenthize 1 $ unwords [functionName, show signal]
        clause     = predReplacedSmt signal fApplied postCond
        constraint = "constraint"

getSygusTargets :: Dto -> Cfg -> [TheorySymbol]
getSygusTargets (Dto _ _ post) cfg = Set.toList intersection
  where outputs      = outputSignals cfg
        postSignals  = Set.fromList $ predSignals post
        intersection = Set.intersection outputs postSignals

-- | Picks one signal to synthesize SyGuS for.
-- Unfortunately, the current procedure only allows synthesis 
-- of one single function. More info:
-- tsltools/docs/tslmt2tsl-limitations.md#simultaneous-updates
pickTarget :: [TheorySymbol] -> TheorySymbol
pickTarget = head

getProductionRules :: TheorySymbol -> Cfg -> Maybe [TAst]
getProductionRules nonterminal cfg = Map.lookup nonterminal (grammar cfg)

nonterminalsUsed :: TheorySymbol -> Cfg -> Set TheorySymbol
nonterminalsUsed symbol cfg = helper symbol Set.empty
  where
    helper :: TheorySymbol -> Set TheorySymbol -> Set TheorySymbol
    helper symbol set =
        case getProductionRules symbol cfg of
          Nothing    -> set
          Just rules -> Set.union set $ Set.fromList $ concat $ map tastSignals rules

-- ((I Int (x y 0 1
--          (+ I I) (- I I)
--          (ite B I I)))
--  (B Bool ((and B B) (or B B) (not B)
--           (= I I) (<= I I) (>= I I))))
--
-- ((x UF (x))
--  (y UF (x y)))

productionRules2Sygus :: TheorySymbol -> [TAst] -> String
productionRules2Sygus nonterminal rules = unlines
  [ minitab 1 $ (parenthize 2 declaration) ++ "("
  , expansion
  , minitab 1 ")"
  ]
  where declaration = unwords [show nonterminal, show (symbolTheory nonterminal)]
        expansion   = minitab 2 $ parenthize 1 $ unwords
          [ declaration
          , parenthize 1 rulesSygus
          ]
        rulesSygus  =  unwords $ map tast2Smt rules

syntaxConstraint :: TheorySymbol -> Cfg -> String
syntaxConstraint functionInput cfg = unlines
  [ funDeclComment
  , "("  ++ functionDeclaration
  , varDeclComment
  , varDecls
  , ""
  , unlines $ fmap sygusGrammar nonterminals
  , ")" ]
  where
    sygusGrammar :: TheorySymbol -> String
    sygusGrammar nonterminal =
      case (getProductionRules nonterminal cfg) of
        Nothing    -> minitab 2 $ ";; No grammar for " ++ show nonterminal ++ "\n"
        Just rules -> productionRules2Sygus nonterminal rules

    declareVar :: TheorySymbol -> String
    declareVar symbol = 
        parenthize 1 $ unwords [show symbol, show (symbolTheory symbol)]

    functionDeclaration = unwords 
      [ "synth-fun"
      , functionName
      , "(("
      , "input"
      , theoryShow
      , "))"
      , theoryShow
      ]
    nonterminals = Set.toList $ nonterminalsUsed functionInput cfg
    theoryShow   = show $ symbolTheory functionInput
    varDecls     = parenthize 1 $ unwords $ map declareVar nonterminals

    funDeclComment = "\r\n;; Name and signature of the function to be synthesized"
    varDeclComment = "\r\n;; Declare the nonterminals used in the grammar"


fixedSizeQuery :: Dto -> Cfg -> Maybe String
fixedSizeQuery dto@(Dto theory pre post) cfg =
  if null sygusTargets
    then Nothing
    else Just $ unlines 
      [declTheory
      , sortDecl
      , grammar
      , preCond
      , postCond
      , checkSynth
      ]
  where
    sygusTargets = getSygusTargets dto cfg
    synthTarget  = pickTarget sygusTargets
    grammar      = syntaxConstraint synthTarget cfg
    preCond      = preCond2Sygus  pre
    postCond     = postCond2Sygus synthTarget post
    declTheory   = "(set-logic " ++ show theory ++ ")"
    checkSynth   = "(check-synth)"
    sortDecl     = smtSortDecl theory

recursiveQuery :: Dto -> Cfg -> String
recursiveQuery = undefined

findRecursion :: [TAst] -> TAst
findRecursion [] = error "Empty list for recursion!"
findRecursion _  = undefined

tast2UpdateChain :: TheorySymbol -> TAst -> String
tast2UpdateChain = undefined
-- (zipWith strConcat nextChains) . tastByDepth
-- where nextChains      = inits $ repeat $ show $ Next 1
--       strConcat s1 s2 = s1 ++ " " ++ s2

sygus2TslAss :: Temporal -> Dto -> TAst -> String
sygus2TslAss = undefined
-- sygus2TslAss temporal (Dto _ pre post) tast = unwords
--   [ "G("
--   , "("
--   , "(" ++ pred2Tsl pre ++ ")"
--   , "&&"
--   , "(" ++ updateTerm ++ ")"
--   , ")"
--   , "->"
--   , show temporal
--   , "(" ++ pred2Tsl post ++ ")"
--   , ";"
--   ]
--   where
--     updateChain = tast2UpdateChain tast
--     updateTerm  = if (temporal == Eventually)
--                      then updateChain ++ " W " ++ pred2Tsl post
--                      else updateChain

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
