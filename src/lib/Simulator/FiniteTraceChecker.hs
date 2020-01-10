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
  , (|=)
  ) where

-----------------------------------------------------------------------------
import Data.List as List (all)
import TSL.Logic as Logic (Formula(..), PredicateTerm, SignalTerm)

-----------------------------------------------------------------------------
--
-- A Finite Trace is List of updates and predicate evalutations 
-- (which are partial functions), a finite trace can be extended by append
--
newtype FiniteTrace c =
  FiniteTrace [(c -> SignalTerm c, PredicateTerm c -> Bool)]

append ::
     FiniteTrace c
  -> (c -> SignalTerm c)
  -> (PredicateTerm c -> Bool)
  -> FiniteTrace c
append (FiniteTrace tr) updates predicates =
  FiniteTrace $ (updates, predicates) : tr

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
(|=) (FiniteTrace []) = \_ -> True
(|=) ts@(FiniteTrace (t:rtr)) =
  \case
    TTrue -> True
    FFalse -> False
    Check p -> (snd t) p
    Update c st -> (fst t) c == st
    Not f -> not $ ts |= f
    And fs -> all (\f -> ts |= f) fs
    Implies f1 f2 -> (not (ts |= f1)) || (ts |= f2)
    Equiv f1 f2 -> (ts |= Implies f1 f2) && (ts |= Implies f2 f1)
    Or fs -> not (all (\f -> not (ts |= f)) fs)
    Next f -> tr |= f
    Globally f -> (ts |= f) && (tr |= Globally f)
    Finally f -> (ts |= f) || (tr |= Finally f)
    Until f1 f2 -> (ts |= f2) || (ts |= f1) && (tr |= Until f1 f2)
    Weak f1 f2 -> (ts |= f2) || (ts |= f1) && (tr |= Weak f1 f2)
    Release f1 f2 -> ts |= Weak f2 (And [f1, f2])
  where
    tr = FiniteTrace rtr
