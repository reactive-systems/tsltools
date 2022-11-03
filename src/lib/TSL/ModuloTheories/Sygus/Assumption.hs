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

import Data.Maybe (catMaybes)

import Data.List (intersperse)

import TSL.Error (Error, errSygus)

import TSL.ModuloTheories.Theories (TheorySymbol, symbol2Tsl)

import TSL.ModuloTheories.Predicates (pred2Tsl)

import TSL.ModuloTheories.Sygus.Common (Dto(..) , Temporal (..), Term)

import TSL.ModuloTheories.Sygus.Update (Update (..), DataSource(..), term2Updates)

-------------------------------------------------------------------------------

removeSelfUpdate :: (Eq a) => Update a -> Maybe (Update a)
removeSelfUpdate update = case (source update) of
    TslFunction _ _   -> Just $ update
    TslValue sinkTerm -> if (sink update) == sinkTerm
                            then Nothing
                            else Just $ update

updates2Tsl :: (Eq a, Show a) => [[Update a]] -> String
updates2Tsl updates = unwords $ intersperse tslAnd depthAssumptions
  where
    tslAnd           = "&&"
    tslNext          = 'X'
    trueUpdates      = getTrueUpdates $ reverse updates
    depthAssumptions = zipWith depth2Assumption [0..] trueUpdates

    getTrueUpdates :: (Eq a) => [[Update a]] -> [[Update a]]
    getTrueUpdates = catMaybes . (map removeSelves)
      where removeSelves xs = case catMaybes ((map removeSelfUpdate) xs) of
                             [] -> Nothing 
                             ys -> Just ys

    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth tslNext
            anded = unwords $ intersperse tslAnd $ map show depthUpdates

makeAssumption :: Temporal -> Dto -> [[Update String]] -> Either Error String
makeAssumption temporal (Dto _ pre post) updates = 
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
    tslAnd      = "&&"
    weakUntil   = " W "
    updateChain = updates2Tsl updates
    updateTerm  = let condition = if temporal == Eventually
                                     then weakUntil ++ pred2Tsl post
                                     else ""
                   in tslAnd ++ updateChain ++ condition
    errMsg      =
      "((p x) & [x <- x]) -> X (p x) type assumptions not yet supported."
