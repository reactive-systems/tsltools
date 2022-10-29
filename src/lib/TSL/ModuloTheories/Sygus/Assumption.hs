-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Assumption
-- Description :  Generate TSL Assumptions from SyGuS results
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Assumption
  ( sygus2TslAssumption
  ) where

-------------------------------------------------------------------------------

import Data.List (intersperse, transpose)

import TSL.ModuloTheories.Sygus.Common (Dto(..)
                                       , Temporal (..)
                                       , Expansion (..)
                                       , Term (..)
                                       )

import TSL.ModuloTheories.Theories (TheorySymbol)

import TSL.ModuloTheories.Predicates (pred2Tsl)

-------------------------------------------------------------------------------

data Update a = Update {sink :: a, source :: DataSource a}

data DataSource a =
      TslValue a
    | TslFunction a [DataSource a]

instance (Show a) => Show (DataSource a) where
  show = \case
    TslValue literal   -> show literal
    TslFunction f args -> show f ++ " " ++ (unwords $ map show args)

update2Tsl :: (Show a) => Update a -> String
update2Tsl (Update dst src) = "[" ++ show dst  ++ " <- " ++ show src ++ "]"

updates2Tsl :: (Show a) => [[Update a]] -> String
updates2Tsl updates = unwords $ zipWith depth2Assumption [0..] $ reverse updates
  where
    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth 'X'
            anded = unwords $ intersperse "&&" $ map update2Tsl depthUpdates

term2DataSource :: Term a -> DataSource a
term2DataSource = \case
  Value value          -> TslValue value
  Expression expansion -> TslValue $ nonterminal expansion
  Function f args      -> TslFunction f $ map term2DataSource args

term2Updates :: Term a -> [[Update a]]
term2Updates = \case
  Value _              -> []
  Expression expansion -> calculateUpdateChain expansion
  Function _ args      -> map concat $ transpose $ map term2Updates args

calculateUpdateChain :: Expansion a -> [[Update a]]
calculateUpdateChain (Expansion dst term) =
  case term of 
    Value      value        -> [[Update dst (TslValue value)]]

    Expression expansion    -> [update]:updates
        where updates = calculateUpdateChain expansion
              update  = Update dst $ TslValue $ nonterminal expansion

    function@(Function _ _) -> [update]:updates
        where updates = term2Updates function
              update  = Update dst $ term2DataSource function

sygus2TslAssumption :: Temporal -> Dto -> Term String -> String
sygus2TslAssumption temporal (Dto _ pre post) term = unwords
  [ "G"
  , "(" -- GLOBALLY
  , "(" -- PRE + UPDATES
  , pred2Tsl pre
  , "&&"
  , updateTerm
  , ")" -- PRE + UPDATES
  , "->"
  , show temporal
  , "(" ++ pred2Tsl post ++ ")"
  , ")" -- GLOBALLY
  , ";"
  ]
  where
    updateChain = updates2Tsl $ term2Updates term
    updateTerm  = if (temporal == Eventually)
                     then updateChain ++ " W " ++ pred2Tsl post
                     else updateChain
