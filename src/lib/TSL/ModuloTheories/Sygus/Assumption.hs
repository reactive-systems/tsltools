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
  (
  ) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Sygus.Common (Dto(..)
                                       , Temporal (..)
                                       , Expansion (..)
                                       , Term (..)
                                       )

import TSL.ModuloTheories.Theories (TheorySymbol)

-------------------------------------------------------------------------------

data Update a = Update {sink :: a, source :: DataSource a}

data DataSource a =
      TSLValue a
    | TSLFunction a [DataSource a]

instance (Show a) => Show (DataSource a) where
  show = \case
    TSLValue literal   -> show literal
    TSLFunction f args -> show f ++ " " ++ (unwords $ map show args)

update2TSLAssumption :: (Show a) => Update a -> String
update2TSLAssumption (Update dst src) = "[" ++ show dst  ++ " <- " ++ show src ++ "]"

sequentialUpdates2TSLAssumption :: (Show a) => [[Update a]] -> String
sequentialUpdates2TSLAssumption updates = "G " ++ assumption ++ ";"
  where
    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth 'X'
            anded = unwords $ intersperse "&&" $ map update2TSLAssumption depthUpdates

    assumption = unwords $ zipWith depth2Assumption [0..] $ reverse updates

term2DataSource :: Term a -> DataSource a
term2DataSource = \case
  Value value          -> TSLValue value
  Expression expansion -> TSLValue $ nonterminal expansion
  Function f args      -> TSLFunction f $ map term2DataSource args

term2Updates :: Term a -> [[Update a]]
term2Updates = \case
  Value value          -> []
  Expression expansion -> calculateUpdateChain expansion
  Function _ args      -> map concat $ transpose $ map term2Updates args

calculateUpdateChain :: Expansion a -> [[Update a]]
calculateUpdateChain (Expansion dst term) =
  case term of 
    Value      value        -> [[Update dst (TSLValue value)]]

    Expression expansion    -> [update]:updates
        where updates = calculateUpdateChain expansion
              update  = Update dst $ TSLValue $ nonterminal expansion

    function@(Function _ _) -> [update]:updates
        where updates = term2Updates function
              update  = Update dst $ term2DataSource function

sygus2TslAssumption :: Temporal -> Dto -> [[Update TheorySymbol]] -> String
sygus2TslAssumption = undefined
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
