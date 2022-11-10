-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Assumption
-- Description :  Generate TSL Assumptions from SyGuS results
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Assumption
  ( makeAssumption
  ) where

-------------------------------------------------------------------------------

import Data.List (intersperse)

import TSL.Error (Error, errSygus)

import TSL.ModuloTheories.Predicates (pred2Tsl)

import TSL.ModuloTheories.Sygus.Common (Dto(..) , Temporal (..))

import TSL.ModuloTheories.Sygus.Update (Update (..))

-------------------------------------------------------------------------------

tslAnd :: String
tslAnd = "&&"

updates2Tsl :: (Eq a, Show a) => [[Update a]] -> String
updates2Tsl updates = unwords $ intersperse tslAnd depthAssumptions
  where
    tslNext          = 'X'
    depthAssumptions = zipWith depth2Assumption [0..] $ reverse updates

    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth tslNext
            anded = unwords $ intersperse tslAnd $ map show depthUpdates

makeAssumption :: Dto -> Temporal -> [[Update String]] -> Either Error String
makeAssumption (Dto _ pre post) temporal updates = 
  if null updateChain
     then errSygus errMsg
     else Right $ unwords [ "G"
                          , "(" -- GLOBALLY
                          , "(" -- PRE + UPDATES
                          , pred2Tsl pre
                          , updateTerm
                          , ")" -- PRE + UPDATES
                          , "->"
                          , show temporal
                          , "(" ++ pred2Tsl post ++ ")"
                          , ")" -- GLOBALLY
                          , ";"
                          ]
  where
    weakUntil   = " W "
    updateChain = updates2Tsl updates
    updateTerm  = let condition = if temporal == Eventually
                                     then weakUntil ++ pred2Tsl post
                                     else ""
                   in unwords [tslAnd, updateChain, condition]
    errMsg      =
      "((p x) & [x <- x]) -> X (p x) type assumptions not yet supported."
