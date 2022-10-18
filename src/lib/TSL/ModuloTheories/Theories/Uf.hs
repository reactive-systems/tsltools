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

data UfSymbol = Uninterpreted String deriving(Eq, Ord)

instance TheorySymbol UfSymbol where
    readT s                 = Right $ Uninterpreted s
    toSmt (Uninterpreted a) = a
    toTsl (Uninterpreted a) = a
    symbolType _            = "UF"
    isUninterpreted _       = True
    makeSignal              = Uninterpreted
