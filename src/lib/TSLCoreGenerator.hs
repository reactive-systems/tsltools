-----------------------------------------------------------------------------
-- |
-- Module      : TSLCoreGenerator
-- Description : TSL unrealizable-core and minimal-assumptions finder
-- Maintainer  : Philippe Heim
--
-- Provides methods to generate unrealizability cores and to search
-- for a minimal amount of assumptions needed to realize a
-- specification.
--
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
module TSLCoreGenerator
  ( generateCore
  , generateMinimalAssumptions
  , treeBasedMinimalAssumptions
  , Context(..)
  , Verbosity(..)
  ) where

-----------------------------------------------------------------------------
import CoreGeneration.UnrealizabilityCores (generateCore)

import CoreGeneration.MinimalAssumptionCores
  ( generateMinimalAssumptions
  , treeBasedMinimalAssumptions
  )

import CoreGeneration.CoreUtilities (Context(..), Verbosity(..))
-----------------------------------------------------------------------------
