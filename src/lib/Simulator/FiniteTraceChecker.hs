-- |
-- Module      :  Simulator.FinitTraceChecker
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple AIGER simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
--
-- TODOs: 
-- - Check purity (although this will always falsify the enivornment)
--
module Simulator.FiniteTraceChecker
  ( FiniteTrace
  , append
  , rewind
  , emptyTrace
  , (|=)
  ) where

-----------------------------------------------------------------------------
import Data.List as List (all)
import TSL.Logic as Logic (Formula(..), PredicateTerm, SignalTerm)

-----------------------------------------------------------------------------
--
-- A Finite Trace is List of updates and predicate evalutations 
-- (which are partial functions), a finite trace can be extended by append,
-- or rewind
--
newtype FiniteTrace c =
  FiniteTrace [(c -> SignalTerm c, PredicateTerm c -> Bool)]

append ::
     FiniteTrace c
  -> (c -> SignalTerm c)
  -> (PredicateTerm c -> Bool)
  -> FiniteTrace c
append (FiniteTrace tr) updates predicates =
  FiniteTrace $ tr ++ [(updates, predicates)]

rewind :: FiniteTrace c -> FiniteTrace c
rewind (FiniteTrace ts) =
  case reverse ts of
    [] -> FiniteTrace []
    (_:tr) -> FiniteTrace (reverse tr)

emptyTrace :: FiniteTrace c
emptyTrace = FiniteTrace []

-----------------------------------------------------------------------------
--
-- This relation is the satisfcation relation of a finite trace of a TSL formula
-- Checking is done by expansion, via
--
--  a U b = b || (a && X (a U b))
--  a W b = b || (a && X (a W b))
--  G a = a && X(G a)
--  F a = a || X(F a)
--  a R b := b W (a && b)
--
(|=) :: Eq c => FiniteTrace c -> Formula c -> Bool
(|=) (FiniteTrace ts) f = check [] ts f

check ::
     Eq c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Formula c
  -> Bool
check [] [] =
  \case
    FFalse -> False
    Previous _ -> False
    _ -> True
check (o:old) [] =
  \case
    FFalse -> False
    Previous f -> check old [o] f
    Once f -> check old [o] (Once f)
    Historically f -> check old [o] (Historically f)
    Since f1 f2 -> check old [o] (Since f1 f2)
    Triggered f1 f2 -> check old [o] (Triggered f1 f2)
    _ -> True
check old ts@(t:tr) =
  \case
    TTrue -> True
    FFalse -> False
    --None temporal
    Check p -> (snd t) p
    Update c st -> (fst t) c == st
    Not f -> not $ check old ts f
    And fs -> all (\f -> check old ts f) fs
    Implies f1 f2 -> (not (check ts old f1)) || (check old ts f2)
    Equiv f1 f2 ->
      (check old ts (Implies f1 f2)) && (check old ts (Implies f2 f1))
    Or fs -> not (all (\f -> not (check old ts f)) fs)
    -- Normal Temporal
    Next f -> check (t : old) tr f
    Globally f -> (check old ts f) && (check (t : old) tr (Globally f))
    Finally f -> (check old ts f) || (check (t : old) tr (Finally f))
    Until f1 f2 ->
      (check old ts f2) ||
      (check old ts f1) && (check (t : old) tr (Until f1 f2))
    Weak f1 f2 ->
      (check old ts f2) ||
      (check old ts f1) && (check (t : old) tr (Weak f1 f2))
    Release f1 f2 -> check old ts (Weak f2 (And [f1, f2]))
    -- Past temoral
    Previous f ->
      case old of
        [] -> False
        (o:ol) -> check ol (o : ts) f
    Once f ->
      (check old ts f) ||
      case old of
        [] -> False
        (o:ol) -> check ol (o : ts) (Once f)
    Historically f ->
      (check old ts f) &&
      case old of
        [] -> True
        (o:ol) -> check ol (o : ts) (Historically f)
    Since f1 f2 ->
      (check old ts f2) ||
      (check old ts f1 &&
       case old of
         [] -> False
         (o:ol) -> check ol (o : ts) (Since f1 f2))
    Triggered f1 f2 ->
      (check old ts f2) &&
      ((check old ts f1) ||
       case old of
         [] -> True
         (o:ol) -> check ol (o : ts) (Triggered f1 f2))
