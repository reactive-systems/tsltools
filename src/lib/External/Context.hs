-----------------------------------------------------------------------------
-- |
-- Module      :  External.Context
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple interface for external tools calls
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module External.Context
  ( Context
  , specSatisfiable
  , specRealizable
  , specSynt
  , specCounterSynt
  , tslSpecSatisfiable
  , tslSpecRealizable
  , tslSpecSynt
  , tslSpecCounterSynt
  ) where

import TSL.Aiger (Circuit)
import TSL.Specification (Specification, TSLSpecification, tslSpecToSpec)
import TSL.TLSF (toTLSF)

-----------------------------------------------------------------------------
--
-- Bundled synthesizing methods assuming the TLSF format
--
data Context =
  Context
    { sat :: String -> IO Bool
    , real :: String -> IO Bool
    , synt :: String -> IO (Maybe Circuit)
    , counterSynt :: String -> IO (Maybe Circuit)
    }

specToTSLF :: Specification -> String
specToTSLF = toTLSF "Specification"

tslSpecToTSLF :: TSLSpecification -> String
tslSpecToTSLF = specToTSLF . tslSpecToSpec

specSatisfiable :: Context -> Specification -> IO Bool
specSatisfiable Context {..} = sat . specToTSLF

tslSpecSatisfiable :: Context -> TSLSpecification -> IO Bool
tslSpecSatisfiable Context {..} = sat . tslSpecToTSLF

specRealizable :: Context -> Specification -> IO Bool
specRealizable Context {..} = real . specToTSLF

tslSpecRealizable :: Context -> TSLSpecification -> IO Bool
tslSpecRealizable Context {..} = real . tslSpecToTSLF

specSynt :: Context -> Specification -> IO (Maybe Circuit)
specSynt Context {..} = synt . specToTSLF

tslSpecSynt :: Context -> TSLSpecification -> IO (Maybe Circuit)
tslSpecSynt Context {..} = synt . tslSpecToTSLF

specCounterSynt :: Context -> Specification -> IO (Maybe Circuit)
specCounterSynt Context {..} = counterSynt . specToTSLF

tslSpecCounterSynt :: Context -> TSLSpecification -> IO (Maybe Circuit)
tslSpecCounterSynt Context {..} = counterSynt . tslSpecToTSLF
