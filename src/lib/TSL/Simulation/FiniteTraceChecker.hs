-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.FiniteTraceChecker
-- Description :  Violation finder in finite TSL traces
-- Maintainer  :  Philippe Heim
--
-- This module tries to check whether some finite TSL trace (consisting of
-- update an predicate evaluation) violates some TSL 'Formula'.
--
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.Simulation.FiniteTraceChecker
  ( FiniteTrace
  , Obligation(..)
  , append
  , rewind
  , emptyTrace
  , violated
  , nextObligations
  ) where

-------------------------------------------------------------------------------
import TSL.Logic as Logic (Formula(..), PredicateTerm, SignalTerm)

import Control.Exception (assert)

import Data.Map as Map (Map, empty, insert, lookup, union)

-------------------------------------------------------------------------------
-- | A 'FiniteTrace' describes a trace of evaluations and 'Obligation's

data FiniteTrace c =
  FiniteTrace
    {
    -- | The trace (anti-chronological) of the update and predicate evaluations
      trace :: [(c -> SignalTerm c, PredicateTerm c -> Bool)]
    -- | The anti-chronological trace of 'Obligation's that have to be
    -- fulfilled
    , obligations :: [[Obligation c]]
    }

-------------------------------------------------------------------------------
-- | An 'Obligation' describes what has to be fulfilled at a certain point.

data Obligation c =
  Obligation
    {
    -- | The guarantee the obligations represents
    -- (this should not be modified over time)
      guarantee :: Formula c
    -- | The guarantee evolved over time (i.e. after some evaluation steps)
    -- with additional assumptions
    , expTotalFormula :: Formula c
    -- | The guarantee evolved over time (i.e. after some evaluation steps)
    -- without assumptions
    , expGuarantee :: Formula c
    }

-------------------------------------------------------------------------------
-- | 'append' extends a 'FiniteTrace' by some predicate and update evaluation.

append ::
     Ord c
  => FiniteTrace c
  -> (c -> SignalTerm c)
  -> (PredicateTerm c -> Bool)
  -> FiniteTrace c
append ft@FiniteTrace {..} updates predicates =
  let newTrace = (updates, predicates) : trace
      newOb =
        fmap
          (\ob@Obligation {expTotalFormula = nextTot, expGuarantee = nextGar} ->
             ob
               { expTotalFormula = checkNext newTrace nextTot
               , expGuarantee = checkNext newTrace nextGar
               })
          (nextObligations ft)
   in ft {trace = newTrace, obligations = newOb : obligations}

-------------------------------------------------------------------------------
-- | 'rewind' undoes the last extensions by 'append'.

rewind :: Ord c => FiniteTrace c -> FiniteTrace c
rewind ft@FiniteTrace {..} =
  case (trace, obligations) of
    ([], _)      -> ft
    (_:tr, _:or) -> ft {trace = tr, obligations = or}
    -- this should never happen as 'append' only add on element to each list
    -- and trace is initially empty
    _ -> assert False undefined

-------------------------------------------------------------------------------
-- | 'emptyTrace' initializes a 'FiniteTrace'. To compute the initial
-- obligation the initial specification has to be passed in form of a list
-- of assumptions and guarantees.

emptyTrace :: Ord c => ([Formula c], [Formula c]) -> FiniteTrace c
emptyTrace (assumptions, guarantees) =
  FiniteTrace
    { trace = []
    , obligations =
        [ fmap
            (\g ->
               Obligation
                 { guarantee = g
                 , expTotalFormula = checkNext [] (Implies (And assumptions) g)
                 , expGuarantee = checkNext [] g
                 })
            guarantees
        ]
    }

-------------------------------------------------------------------------------
-- | 'violated' returns the 'Formula' that some finite trace violates.
violated :: Eq c => FiniteTrace c -> [Formula c]
violated ft =
  guarantee <$> filter ((== FFalse) . expTotalFormula) (nextObligations ft)

-------------------------------------------------------------------------------
-- | 'nextObligation' returns the obligation that has to be fulfilled with the
-- next evaluation step.
nextObligations :: FiniteTrace c -> [Obligation c]
nextObligations FiniteTrace {..} =
  case obligations of
    o:_ -> o
    -- this should never happen as there is always an initial obligation
    [] -> assert False undefined

-------------------------------------------------------------------------------
-- | 'checkNext' expands a 'Formula' by a single evaluation step by applying
-- and simplifying temporal operations. E.g. if the 'Formula' has the form
-- "F [x <- t]" and the last evaluation contained "[x <- t]" the formula should
-- be simplified to "true".
checkNext ::
     Ord c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Formula c
  -> Formula c
checkNext trace form = fst $ checkNextC trace empty form

-- To improve performance, the computation is enhanced with caching for
-- intermediate results.
checkNextC ::
     Ord c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Map (Formula c) (Formula c)
  -> Formula c
  -> (Formula c, Map (Formula c) (Formula c))
checkNextC [] cache form =
  -- Even if the trace is empty some past operators simplify trivially
  case form of
    Historically _ -> (TTrue, cache)
    Triggered _ _  -> (TTrue, cache)
    f              -> (f, cache)
checkNextC ts@(t:tr) cache form =
  let simpForm = simplify form
   in case Map.lookup simpForm cache of
        Just f -> (f, cache)
        Nothing ->
          let (nextForm, cache') =
                case form of
                  -- Atomar expressions can be evaluated immediately
                  TTrue -> (TTrue, empty)
                  FFalse -> (FFalse, empty)
                  Check p ->
                    if snd t p
                      then (TTrue, empty)
                      else (FFalse, empty)
                  Update c st ->
                    if fst t c == st
                      then (TTrue, empty)
                      else (FFalse, empty)
                  -- Boolean Operators
                  Not f ->
                    let (f', c) = checkNextC ts cache f
                     in (Not f', c)
                  And fs ->
                    let (fs', c) =
                          foldl
                            (\(fr, c) e ->
                               let (f', c') = checkNextC ts c e
                                in (f' : fr, c'))
                            ([], cache)
                            (reverse fs)
                     in (And fs', c)
                  Or fs ->
                    let (fs', c) =
                          foldl
                            (\(fr, c) e ->
                               let (f', c') = checkNextC ts c e
                                in (f' : fr, c'))
                            ([], cache)
                            (reverse fs)
                     in (Or fs', c)
                  -- Essential temporal operators. Note that the past operators
                  -- have to delete the caching when they consider older states
                  -- of the trace as.
                  Next f -> (f, cache)
                  Previous f ->
                    case tr of
                      [] -> (FFalse, empty)
                      _  -> checkNextC ts cache $ fst $ checkNextC tr empty f
                  Historically f ->
                    checkNextC ts cache $
                    And [f, fst $ checkNextC tr empty (Historically f)]
                  Triggered f1 f2 ->
                    checkNextC ts cache $
                    And
                      [f2, Or [f1, fst $ checkNextC tr empty (Triggered f1 f2)]]
                  -- The following operators are expanded using either an
                  -- equivalent already defined form or the standard temporal
                  -- expansion rules. The necessary operators have already been
                  -- handled.
                  Implies f1 f2 -> checkNextC ts cache $ Or [Not f1, f2]
                  Equiv f1 f2 ->
                    checkNextC ts cache $ And [Implies f1 f2, Implies f2 f1]
                  Globally f -> checkNextC ts cache $ And [f, Next (Globally f)]
                  Finally f -> checkNextC ts cache $ Or [f, Next (Finally f)]
                  Until f1 f2 ->
                    checkNextC ts cache $ Or [f2, And [f1, Next (Until f1 f2)]]
                  Weak f1 f2 ->
                    checkNextC ts cache $ Or [f2, And [f1, Next (Until f1 f2)]]
                  Release f1 f2 -> checkNextC ts cache $ Weak f2 (And [f1, f2])
                  Once f -> checkNextC ts cache $ Or [f, Previous (Once f)]
                  Since f1 f2 ->
                    checkNextC ts cache $
                    Or [f2, And [f1, Previous (Since f1 f2)]]
           in ( simplify nextForm
              , insert simpForm (simplify nextForm) (cache `union` cache'))

-------------------------------------------------------------------------------
-- | 'simplify' simplifies a TSL 'formula' by applying some easy syntactic
-- conversion on the boolean level.

simplify :: Eq c => Formula c -> Formula c
simplify =
  \case
    Not f ->
      case simplify f of
        TTrue  -> FFalse
        FFalse -> TTrue
        f'     -> Not f'
    And [] -> TTrue
    And [f] -> simplify f
    And fs ->
      let fs' = map simplify fs
       in if any isFalse fs'
            then FFalse
            else And $
                 removeDoubles $
                 foldl
                   (\xs e ->
                      case e of
                        And g -> g ++ xs
                        TTrue -> xs
                        g     -> g : xs)
                   []
                   fs'
    Or [] -> FFalse
    Or [f] -> simplify f
    Or fs ->
      let fs' = map simplify fs
       in if any isTrue fs'
            then TTrue
            else Or $
                 removeDoubles $
                 foldl
                   (\xs e ->
                      case e of
                        Or g   -> g ++ xs
                        FFalse -> xs
                        g      -> g : xs)
                   []
                   fs'
    Implies f1 f2 ->
      case (simplify f1, simplify f2) of
        (FFalse, _) -> TTrue
        (TTrue, f)  -> f
        (f1', f2')  -> Implies f1' f2'
    Equiv f1 f2 ->
      case (simplify f1, simplify f2) of
        (FFalse, f) -> simplify (Not f)
        (f, FFalse) -> simplify (Not f)
        (f, TTrue)  -> f
        (TTrue, f)  -> f
        (f1', f2')  -> Equiv f1' f2'
    f -> f
  where
    isFalse FFalse = True
    isFalse _      = False
    --
    isTrue TTrue = True
    isTrue _     = False
    --
    removeDoubles :: Eq a => [a] -> [a]
    removeDoubles [] = []
    removeDoubles (x:xr) =
      if x `elem` xr
        then removeDoubles xr
        else x : removeDoubles xr

-------------------------------------------------------------------------------
