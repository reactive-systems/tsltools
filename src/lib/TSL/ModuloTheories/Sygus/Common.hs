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
  , Expansion (..)
  , Term (..)
  , Model (..)
  , IntermediateResults (..)
  , targetPostfix
  , parenthize
  ) where

-------------------------------------------------------------------------------

import TSL.ModuloTheories.Theories (Theory)

import TSL.ModuloTheories.Predicates (TheoryPredicate)

-------------------------------------------------------------------------------

targetPostfix :: String
targetPostfix = "_target"

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

instance Show Dto where
  show Dto{..} = unlines [ "DTO:"
                         , '\t':show preCondition
                         , '\t':show postCondition
                         , ""
                         ]

data Expansion a = Expansion {nonterminal :: a, rule :: Term a} deriving (Show)

instance Functor Expansion where
  fmap f Expansion{..} = Expansion (f nonterminal) $ fmap f rule

data Term a = 
      Value a
    | Expression (Expansion a)
    | Function a [Term a]
    deriving (Show)

instance Functor Term where
  fmap f = \case
    Value v            -> Value $ f v 
    Expression e       -> Expression (fmap f e)
    Function func args -> Function (f func) $ map (fmap f) args

data IntermediateResults = IntermediateResults
  {  problem :: String
  ,  query   :: String
  ,  result  :: String
  }
  deriving (Show)

newtype Model a = Model (a,a) deriving (Show)

parenthize :: Int -> String -> String
parenthize repeats str = lpars ++ str ++ rpars
  where lpars = replicate repeats '(' 
        rpars = replicate repeats ')'
