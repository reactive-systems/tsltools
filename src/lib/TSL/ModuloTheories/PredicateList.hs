-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.PredicateList
-- Description :  Predicate term operations for TSL-MT
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.PredicateList( PredicateLiteral
                                       , getPredicateLiterals
                                       , enumeratePreds
                                       , toSMT2) where

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

-- | TODO: make AST type more granular to enforce
-- the fact that PredicateLiteral cannot be constructed
-- out of PredicateTerms.
data PredicateLiteral a =
      PLiteral (AST a)
    | NotPLit (PredicateLiteral a)
    | OrPLit  (PredicateLiteral a) (PredicateLiteral a)
    | AndPLit (PredicateLiteral a) (PredicateLiteral a)

instance Show a => Show (PredicateLiteral a) where
  show = \case
    PLiteral p  -> show p     
    NotPLit p   -> "!" ++ show p ++ ""
    OrPLit p q  -> "(" ++ show p ++ " || " ++ show q ++ ")"
    AndPLit p q -> "(" ++ show p ++ " && " ++ show q ++ ")"

instance Functor PredicateLiteral where
  fmap f = \case
    PLiteral p  -> PLiteral $ fmap f p
    NotPLit p   -> NotPLit $ fmap f p
    OrPLit p q  -> OrPLit (fmap f p) (fmap f q)
    AndPLit p q -> AndPLit (fmap f p) (fmap f q)

toSMT2 :: Show a => PredicateLiteral a -> String
toSMT2 = \case
  PLiteral p  -> show p
  NotPLit p   -> "(not " ++ show p ++ ")"
  OrPLit p q  -> "(or " ++ show p ++ " " ++ show q ++ ")"
  AndPLit p q -> "(and " ++ show p ++ " " ++ show q ++ ")"

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
