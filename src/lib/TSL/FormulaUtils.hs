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
  , getChecks
  , getPredicates
  , constantTrue
  , constantFalse
  , conjunctFormulas
  , disjunctFormulas
  , negateFormula
  , getInputs
  ) where

-----------------------------------------------------------------------------
import Data.Set

import TSL.Logic
  ( Formula(..)
  , FunctionTerm(..)
  , PredicateTerm(..)
  , SignalTerm(..)
  )

-----------------------------------------------------------------------------
-- | Wrap some basic logical operators
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
-- | Get update parts of a formula
getUpdates :: Ord c => Formula c -> Set (Formula c)
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
-- | Get output streams of a formula
getOutputs :: Ord c => Formula c -> Set c
getOutputs form =
  Data.Set.map
    (\case
       Update s _ -> s
       _ -> undefined -- In this case get Updates has to be wrong !!
     )
    (getUpdates form)

-----------------------------------------------------------------------------
-- | Get check parts of a formula
getChecks :: Ord c => Formula c -> Set (Formula c)
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
-- | Get predicate terms out of formula
getPredicates :: Ord c => Formula c -> Set (PredicateTerm c)
getPredicates form =
  Prelude.foldl
    (\set ->
       \case
         Check p -> insert p set
         _ -> undefined -- In this case get Checks has to be wrong !!
     )
    empty
    (getChecks form)

-----------------------------------------------------------------------------
-- | Extract inputs out of predicate terms  out of a formula
getInputs :: Ord c => Formula c -> Set c
getInputs form =
  Prelude.foldl
    (\set ->
       \case
         Check s -> union set (getSignal s)
         _ -> undefined -- In this case get Checks has to be wrong !!
     )
    empty
    (getChecks form)

-----------------------------------------------------------------------------
-- | Extract inputs out of a predicate terms
getSignal :: Ord c => PredicateTerm c -> Set c
getSignal =
  \case
    BooleanInput a -> singleton a
    PApplied a b -> union (getSignal a) (getInput b)
    _ -> empty

-----------------------------------------------------------------------------
-- | Extract all streams out of a signal term
getInput :: Ord c => SignalTerm c -> Set c
getInput =
  \case
    Signal a -> singleton a
    PredicateTerm a -> getSignal a
    FunctionTerm a -> getSigFunc a

-----------------------------------------------------------------------------
-- | Extract all streams out of a function term
getSigFunc :: Ord c => FunctionTerm c -> Set c
getSigFunc =
  \case
    FApplied a b -> union (getSigFunc a) (getInput b)
    _ -> empty

-----------------------------------------------------------------------------
-- | Get all possible updates from a set of possible outputs
getPossibleUpdates :: Ord c => Formula c -> Set c -> Set (Formula c)
getPossibleUpdates form outps =
  let updates = getUpdates form
      selfUpdates =
        fromList [Update s (Signal s) | s <- toList (getOutputs form)]
      filt (Update s _) = member s outps
      filt _ = False
   in Data.Set.filter filt $ union updates selfUpdates
