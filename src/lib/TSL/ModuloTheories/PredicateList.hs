-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.PredicateList
-- Description :  Predicate term operations for TSL-MT
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.PredicateList(getPredicateTerms) where

-------------------------------------------------------------------------------

import TSL.Specification(Specification(..))

import TSL.SymbolTable(Id, SymbolTable(..))

import TSL.Types(arity)

import TSL.Logic( Formula(..)
                , PredicateTerm(..)
                , foldFormula
                )

import TSL.ModuloTheories.AST(AST, fromPredicateTerm)

-------------------------------------------------------------------------------

getPredicateTerms :: Specification -> [AST Id]
getPredicateTerms (Specification a g s) = map fromPTerm pTerms
  where 
    pTerms    = (fromFList a []) ++ (fromFList g [])
    fromPTerm = fromPredicateTerm (arity . (stType s))

fromFList :: [Formula a] -> [PredicateTerm a] -> [PredicateTerm a]
fromFList [] ps     = ps
fromFList (x:xs) ps = fromFList xs (foldFormula getPredsInF ps x)

-- | Obtains all predicate terms in a given TSL-MT formula
getPredsInF :: Formula a -> [PredicateTerm a] -> [PredicateTerm a]
getPredsInF (Check p) ps = p:ps
getPredsInF _ ps         = ps
