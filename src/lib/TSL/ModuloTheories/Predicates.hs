-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Predicates
-- Description :  Predicate term operations for TSL-MT
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Predicates( TheoryPredicate(..)
                                       , predsFromSpec
                                       , enumeratePreds
                                       ) where

-------------------------------------------------------------------------------

import TSL.Error(Error)

import TSL.Specification(Specification(..))

import TSL.SymbolTable(SymbolTable(..))

import TSL.Types(arity)

import TSL.Logic(PredicateTerm, Formula(..))

import TSL.Ast(fromPredicateTerm)

import TSL.ModuloTheories.Theories(Theory, TAst, applySemantics)

-------------------------------------------------------------------------------

data TheoryPredicate =
      PLiteral TAst
    | NotPLit  TheoryPredicate 
    | OrPLit   TheoryPredicate TheoryPredicate
    | AndPLit  TheoryPredicate TheoryPredicate
    deriving (Show)

-- FIXME: make this tractable
enumeratePreds :: [TheoryPredicate] -> [TheoryPredicate]
enumeratePreds preds = iterateAll $ preds ++ map (NotPLit) preds
  where
    iterateAll []     = []
    iterateAll (x:xs) = map (AndPLit x) xs ++ iterateAll xs

predsFromSpec :: Theory -> Specification -> Either Error [TheoryPredicate]
predsFromSpec theory (Specification a g s) = mapM toTheoryPred asts
  where 
    pTerms       = concat $ map fromFormula $ a ++ g
    unhash       = stName s
    toAst        = fromPredicateTerm (arity . (stType s))
    asts         = map ((fmap unhash) . toAst) pTerms
    toTheoryPred = (fmap PLiteral) . (applySemantics theory)

fromFormula :: Formula a -> [PredicateTerm a]
fromFormula = baseCaseExtender []
  where baseCaseExtender ps (Check p) = p:ps
        baseCaseExtender ps _         = ps
