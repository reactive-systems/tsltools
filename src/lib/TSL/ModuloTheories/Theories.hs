-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Theories
-- Description :  Supported First-Order Theories.
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Theories( Theory(..)
                                  , TheorySymbol(..)
                                  , readTheory
                                  , applySemantics
                                  ) where
-------------------------------------------------------------------------------

import TSL.ModuloTheories.AST(AST(..))

import TSL.ModuloTheories.Theories.Base( TheoryParseErr
                                       , Theory(..)
                                       , TheorySymbol(..)
                                       )

import TSL.ModuloTheories.Theories.Uf(UfTheory)
import TSL.ModuloTheories.Theories.Lia(LiaTheory)

-------------------------------------------------------------------------------
readTheory :: String -> Either TheoryParseErr Theory
readTheory "UF"  = Right UfTheory
readTheory "LIA" = Right LiaTheory
readTheory _     = Left TheoryParseErr
