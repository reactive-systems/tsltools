-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.CFG
-- Description :  Builds a CFG for cells and outputs from the specification.
-- Maintainer  :  Wonhyuk Choi
--
-- This module builds a Context-Free Grammar for cell and output signals
-- from the original specification.
-- This is necessary to build a Syntax-Guided Synthesis grammar when
-- transforming a TSL-MT specification to TSL.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.CFG
  ( 
  ) where
