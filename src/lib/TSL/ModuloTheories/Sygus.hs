-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus
-- Description :  Master module for Sygus
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus
  (
  ) where

-------------------------------------------------------------------------------

import Control.Monad.Trans.Except

import TSL.Error (Error)

import TSL.ModuloTheories.Cfg (Cfg)

import TSL.ModuloTheories.Sygus.Common (Temporal, Dto)

-------------------------------------------------------------------------------

-- | A SyGuS Query is based off of:
-- 1) Data Transformation Obligation (the "semantic  constraint") and
-- 2) Context-Free Grammar           (the "syntactic constraint")
sygusAssumptions
  :: (Int -> String -> ExceptT Error IO String)
  -> (Temporal, Dto)
  -> Cfg
  -> ExceptT Error IO String
sygusAssumptions sygusSolver (temporal, dto) cfg = undefined
