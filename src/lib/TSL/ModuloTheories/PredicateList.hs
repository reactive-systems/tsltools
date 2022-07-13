-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.PredicateList
-- Description :  Predicate term operations for TSL-MT
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.PredicateList( PredicateLiteral(..)
                                       , getPredicateLiterals
                                       , enumeratePreds
                                       , getPLitVars
                                       ) where

-------------------------------------------------------------------------------

import TSL.Specification(Specification(..))

import TSL.SymbolTable(Id, SymbolTable(..))

import TSL.Types(arity)

import TSL.Logic( Formula(..)
                , PredicateTerm(..)
                , foldFormula
                )

import TSL.Ast(Ast, fromPredicateTerm, getVars)

-------------------------------------------------------------------------------

data PredicateLiteral a =
      PLiteral (Ast a)
    | NotPLit  (PredicateLiteral a)
    | OrPLit   (PredicateLiteral a) (PredicateLiteral a)
    | AndPLit  (PredicateLiteral a) (PredicateLiteral a)
    deriving (Show)

instance Functor PredicateLiteral where
  fmap f = \case
    PLiteral ast -> PLiteral $ fmap f ast
    NotPLit p    -> NotPLit  $ fmap f p
    OrPLit p q   -> OrPLit  (fmap f p) (fmap f q)
    AndPLit p q  -> AndPLit (fmap f p) (fmap f q)

instance Foldable PredicateLiteral where
  foldr f acc = \case
    PLiteral ast -> foldr f acc ast
    NotPLit p    -> foldr f acc p
    OrPLit p q   -> foldr f (foldr f acc q) p
    AndPLit p q  -> foldr f (foldr f acc q) p

-- FIXME: make this tractable
getPLitVars :: PredicateLiteral a -> [a]
getPLitVars = \case
  PLiteral ast -> getVars ast
  NotPLit p    -> getPLitVars p
  OrPLit p q   -> getPLitVars p ++ getPLitVars q
  AndPLit p q  -> getPLitVars p ++ getPLitVars q

-- FIXME: make this tractable
enumeratePreds :: [PredicateLiteral a] -> [PredicateLiteral a]
enumeratePreds preds = iterateAll $ preds ++ map (NotPLit) preds
  where
    iterateAll []     = []
    iterateAll (x:xs) = map (AndPLit x) xs ++ iterateAll xs

getPredicateLiterals :: Specification -> [PredicateLiteral Id]
getPredicateLiterals (Specification a g s) = map toPLTerm pTerms
  where 
    toPLTerm  p = PLiteral (fromPTerm p)
    pTerms      = (fromFList a []) ++ (fromFList g [])
    fromPTerm   = fromPredicateTerm (arity . (stType s))

fromFList :: [Formula a] -> [PredicateTerm a] -> [PredicateTerm a]
fromFList [] ps     = ps
fromFList (x:xs) ps = fromFList xs (foldFormula getPredsInF ps x)

-- | Obtains all predicate terms in a given TSL-MT formula
getPredsInF :: Formula a -> [PredicateTerm a] -> [PredicateTerm a]
getPredsInF (Check p) ps = p:ps
getPredsInF _ ps         = ps
