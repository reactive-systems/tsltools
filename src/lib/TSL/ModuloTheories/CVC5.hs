-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.CVC5
-- Description :  Utilities to send SMT and SyGuS problems to CVC5.
-- Maintainer  :  Wonhyuk Choi
--
-- Used for sending SMT and SyGuS problems to CVC5.

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-
   TODO:
   * Send SMT/SyGuS problems to CVC5
   * Parse results from CVC5
   * Transform CVC5 results into some internal function representation

   N.B.
   * This module should have EVERYTHING related to CVC5.
   * _However_, it should not have String -> SMTLib or String -> SyGuS2.0 functions,
   since those are not necessarily tied to CVC5.
   * Those functions should live in their respective modules,
   i.e. Sygus.hs and ConsistencyChecking.hs.
-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.CVC5
  ( 
  ) where
