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
  ( CFG(..)
  , 
  ) where

-------------------------------------------------------------------------------

import TSL.Specification (Specification)

import qualified Data.Sequence as Seq

import Data.Sequence((!?), (><), Seq(..), (|>), fromList)

-------------------------------------------------------------------------------

type Id = Int

data CFG = CFG
    { -- | CFG implemented as a seq of lists.
      -- To get the possible gramamrs for each,
      -- index into the grammar with the signal Id.
        grammar :: Seq.Seq [Formula Id]
    , -- | Converts Id to name.
        toName :: (Id -> String)
    }

fromSpec :: Specification -> CFG
fromSpec (Specification a g s) =
    CFG { grammar = buildGrammar
        , toName = stName s
        }

-- TODO: just start out with static length
buildGrammar :: [Formula Id] -> Seq.Seq [Formula Id]
buildGrammar = buildGrammar' (Seq.fromList [])
  where 
    buildGrammar' acc []     = acc
    buildGrammar' acc (x:xs) = 
        \case x of
          (Update dst src) -> undefined
          -- TODO: append src to acc[dst]
           _               -> undefined
