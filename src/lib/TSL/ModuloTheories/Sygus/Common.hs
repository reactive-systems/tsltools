-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Common
-- Description :  Common data structures for SyGuS
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Common
  ( Temporal (..)
  , Dto (..)
  ) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories( Theory )

-------------------------------------------------------------------------------

data Temporal =
      Next Int
    | Eventually
    deriving (Eq)

instance Show Temporal where
  show = \case
    Next numNext -> replicate numNext 'X'
    Eventually   -> "F"

-- | Data Transformation Obligation.
data Dto = Dto 
    {   theory        :: Theory
    ,   preCondition  :: TheoryPredicate
    ,   postCondition :: TheoryPredicate
    }

data Expansion a = Expansion {nonterminal :: a, rule :: Term a} deriving (Show)

data Term a = 
      Value a
    | Expression (Expansion a)
    | Function a [Term a]
    deriving (Show)
