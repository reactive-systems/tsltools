-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.AST
-- Description :  
-- Maintainer  :  Wonhyuk Choi
--

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.AST
  ) where

-------------------------------------------------------------------------------

import TSL.Logic ( Formula(..)
                 , SignalTerm(..)
                 )

import Types(arity)

import TSL.Specification (Specification(..))

import TSL.SymbolTable (Id, SymbolTable(..), Kind(..))

-------------------------------------------------------------------------------

data AST = AST
