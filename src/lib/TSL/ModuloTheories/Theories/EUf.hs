-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories.EUf
-- Description :  Theory of equality logic with uninterpreted functions (EUF)
-- Maintainer  :  Wonhyuk Choi
-- For a primer on EUF, check https://www21.in.tum.de/teaching/sar/SS20/6.pdf.

-------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase      #-}

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Theories.EUf(EUfSymbol) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories.Base(TheorySymbol(..))

-------------------------------------------------------------------------------

data EUfSymbol = 
      Eq
    | Uninterpreted String 
    deriving(Eq, Ord)

instance TheorySymbol EUfSymbol where
    readT = \case
      "eq"          -> Right Eq
      uninterpreted -> Right $ Uninterpreted uninterpreted

    toSmt = \case
      Eq                -> "="
      Uninterpreted str -> str

    toTsl = \case
      Eq                -> "eq"
      Uninterpreted str -> str

    symbolType _ = "EUF"

    isUninterpreted = \case
      Eq              -> False
      Uninterpreted _ -> True

    makeSignal = Uninterpreted
