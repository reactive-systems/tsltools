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
  , constantTrue
  , constantFalse
  , conjunctFormulas
  , disjunctFormulas
  , negateFormula
  , getInputs
  ) where

-----------------------------------------------------------------------------
import Data.Set

import TSL.Logic (Formula(..), SignalTerm(..), PredicateTerm(..), FunctionTerm(..))

-----------------------------------------------------------------------------
--
-- Wrap some basic logical operators
--
conjunctFormulas :: [Formula a] -> Formula a
conjunctFormulas = And

disjunctFormulas :: [Formula a] -> Formula a
disjunctFormulas = Or

negateFormula :: Formula a -> Formula a
negateFormula = Not

constantFalse :: Formula a
constantFalse = FFalse

constantTrue :: Formula a
constantTrue = TTrue

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
    Previous x -> getUpdates x
    Globally x -> getUpdates x
    Finally x -> getUpdates x
    Once x -> getUpdates x
    Historically x -> getUpdates x
    Until x y -> union (getUpdates x) (getUpdates y)
    Release x y -> union (getUpdates x) (getUpdates y)
    Weak x y -> union (getUpdates x) (getUpdates y)
    Since x y -> union (getUpdates x) (getUpdates y)
    Triggered x y -> union (getUpdates x) (getUpdates y)

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
getChecks :: Formula Int -> Set (Formula Int)
getChecks =
  \case
    TTrue -> empty
    FFalse -> empty
    Check s -> singleton (Check s) 
    Update _ _ -> empty
    Not x -> getChecks x
    Implies x y -> union (getChecks x) (getChecks y)
    Equiv x y -> union (getChecks x) (getChecks y)
    And xs -> unions (fmap getChecks xs)
    Or xs -> unions (fmap getChecks xs)
    Next x -> getChecks x
    Previous x -> getChecks x
    Globally x -> getChecks x
    Finally x -> getChecks x
    Once x -> getChecks x
    Historically x -> getChecks x
    Until x y -> union (getChecks x) (getChecks y)
    Release x y -> union (getChecks x) (getChecks y)
    Weak x y -> union (getChecks x) (getChecks y)
    Since x y -> union (getChecks x) (getChecks y)
    Triggered x y -> union (getChecks x) (getChecks y)

-----------------------------------------------------------------------------
getInputs :: Formula Int -> Set Int
getInputs form =
  Prelude.foldl
    (\set ->
      \case
       Check s -> maybe set ((flip Data.Set.insert) set) (getSignal s)
       _ -> undefined -- In this case get Checks has to be wrong !!
     )
    Data.Set.empty
    (getChecks form)

-----------------------------------------------------------------------------
getSignal :: PredicateTerm Int -> Maybe Int
getSignal =
  \case
    BooleanInput a  -> Just a
    PApplied _ b    -> getInput b
    _               -> Nothing


-----------------------------------------------------------------------------
getInput :: SignalTerm Int -> Maybe Int
getInput =
  \case
    Signal a        -> Just a
    PredicateTerm a -> getSignal a
    FunctionTerm  a -> getSigFunc a


-----------------------------------------------------------------------------
getSigFunc :: FunctionTerm Int -> Maybe Int
getSigFunc =
  \case
    FApplied _ b -> getInput b
    _ -> Nothing
    
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
