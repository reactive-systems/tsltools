-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Debug
-- Description :  Data Structures for Debugging
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------

module TSL.ModuloTheories.Debug (IntermediateResults (..)) where

-------------------------------------------------------------------------------

data IntermediateResults = IntermediateResults
  {  problem    :: String
  ,  query      :: String
  ,  result     :: String
  }
  deriving (Show)
