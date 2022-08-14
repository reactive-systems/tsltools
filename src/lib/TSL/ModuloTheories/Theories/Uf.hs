-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.Uf
-- Description :  Uninterpreted Functions
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.Uf(UfSymbol) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories.Base(TheorySymbol(..))

-------------------------------------------------------------------------------

data UfSymbol = Uninterpreted String deriving (Show)

instance TheorySymbol UfSymbol where
    readT s                 = Right $ Uninterpreted s
    toSmt (Uninterpreted a) = show a
    toTsl (Uninterpreted a) = show a
    symbolType _            = "UFVar"
