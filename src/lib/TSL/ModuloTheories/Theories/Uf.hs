-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.Uf
-- Description :  Uninterpreted Functions
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories.Uf(UfTheory) where

-------------------------------------------------------------------------------
import TSL.ModuloTheories.Theories.Base( TheoryParseErr(..)
                                       , Theory
                                       , TheorySymbol
                                       , readT
                                       , applySemantics
                                       , toSMT2
                                       , toTSL
                                       )
-------------------------------------------------------------------------------
data UfTheory = UfTheory deriving (Show, Eq)

instance Theory UfTheory where
  applySemantics _ = fmap readT

data UfSymbol = Uninterpreted String deriving (Show)

instance TheorySymbol UfSymbol where
    readT s                  = Right $ Uninterpreted s
    toSMT2 (Uninterpreted a) = show a
    toTSL  (Uninterpreted a) = show a
