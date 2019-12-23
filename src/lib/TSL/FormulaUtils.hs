-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.FormulaUtils
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Some simple utilities on tsl formulas
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.FormulaUtils
  ( getPossibleUpdates
  , getUpdates
  , getOutputs
  ) where

-----------------------------------------------------------------------------
import Data.Set

import TSL.Logic (Formula(..), SignalTerm(..))

-----------------------------------------------------------------------------
getUpdates :: Formula Int -> Set (Formula Int)
getUpdates =
  \case
    TTrue -> empty
    FFalse -> empty
    Check _ -> empty
    Update s t -> singleton (Update s t)
    Not x -> getUpdates x
    Implies x y -> union (getUpdates x) (getUpdates y)
    Equiv x y -> union (getUpdates x) (getUpdates y)
    And xs -> unions (fmap getUpdates xs)
    Or xs -> unions (fmap getUpdates xs)
    Next x -> getUpdates x
    Globally x -> getUpdates x
    Finally x -> getUpdates x
    Until x y -> union (getUpdates x) (getUpdates y)
    Release x y -> union (getUpdates x) (getUpdates y)
    Weak x y -> union (getUpdates x) (getUpdates y)

-----------------------------------------------------------------------------
getOutputs :: Formula Int -> Set Int
getOutputs form =
  Data.Set.map
    (\case
       Update s _ -> s
       _ -> undefined -- In this case get Updates has to be wrong !!
     )
    (getUpdates form)

-----------------------------------------------------------------------------
--
-- Get all possible updates from a set of possible outputs
--
getPossibleUpdates :: Formula Int -> Set Int -> Set (Formula Int)
getPossibleUpdates form outps =
  let updates = getUpdates form
      selfUpdates =
        fromList [Update s (Signal s) | s <- toList (getOutputs form)]
      filt (Update s _) = member s outps
      filt _ = False
   in Data.Set.filter filt $ union updates selfUpdates
