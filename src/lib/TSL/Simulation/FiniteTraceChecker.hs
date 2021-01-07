-- |
-- Module      :  TSL.Simulation.FinitTraceChecker
-- Maintainer  :  Philippe Heim
--
-- A simple finite trace checker
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.FiniteTraceChecker
  ( FiniteTrace
  , Obligation(..)
  , append
  , rewind
  , emptyTrace
  , violated
  , nextObligations
  ) where

-----------------------------------------------------------------------------
import TSL.Logic as Logic (Formula(..), PredicateTerm, SignalTerm)

import Control.Exception (assert)

import Data.Map as Map (Map, empty, insert, lookup, union)

-----------------------------------------------------------------------------
-- | A Finite Trace is a stack of updates and predicate evalutations 
-- (which are partial functions), a finite trace can be extended by append,
-- or rewind and the specification that should be fulfilled
--
data FiniteTrace c =
  FiniteTrace
    { trace :: [(c -> SignalTerm c, PredicateTerm c -> Bool)]
    , obligations :: [[Obligation c]]
    }

data Obligation c =
  Obligation
    { guarantee :: Formula c
    , expTotalFormula :: Formula c
    , expGuarantee :: Formula c
    }

-----------------------------------------------------------------------------
-- | Adds an update and predicate evaluation at the end of a finite trace
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

-----------------------------------------------------------------------------
-- | Reverts the last appending to the finite trace. If the trace is empty
-- the trace stays empty
rewind :: Ord c => FiniteTrace c -> FiniteTrace c
rewind ft@FiniteTrace {..} =
  case (trace, obligations) of
    ([], _) -> ft
    (_:tr, _:or) -> ft {trace = tr, obligations = or}
    _ -> assert False undefined

-----------------------------------------------------------------------------
-- | The empty finite trace
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

-----------------------------------------------------------------------------
-- | This function returns the violated guarantees
violated :: Eq c => FiniteTrace c -> [Formula c]
violated ft =
  fmap guarantee $ filter ((== FFalse) . expTotalFormula) (nextObligations ft)

-----------------------------------------------------------------------------
-- | The next obligations of the trace
nextObligations :: FiniteTrace c -> [Obligation c]
nextObligations FiniteTrace {..} =
  case obligations of
    [] -> assert False undefined
    o:_ -> o

-----------------------------------------------------------------------------
-- | Given a formula and a trace calculates (using (local time) caching) the next 
-- obliation
checkNext ::
     Ord c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Formula c
  -> Formula c
checkNext trace form = fst $ checkNextC trace empty form

checkNextC ::
     Ord c
  => [(c -> SignalTerm c, PredicateTerm c -> Bool)]
  -> Map (Formula c) (Formula c)
  -> Formula c
  -> (Formula c, Map (Formula c) (Formula c))
checkNextC [] cache form =
  case form of
    Historically _ -> (TTrue, cache)
    Triggered _ _ -> (TTrue, cache)
    f -> (f, cache)
checkNextC ts@(t:tr) cache form =
  let simpForm = simplify form
   in case Map.lookup simpForm cache of
        Just f -> (f, cache)
        Nothing ->
          let (nextForm, cache') =
                case form of
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
                  Next f -> (f, cache)
                  Previous f ->
                    case tr of
                      [] -> (FFalse, empty)
                      _ -> checkNextC ts cache $ fst $ checkNextC tr empty f
                  Historically f ->
                    checkNextC ts cache $
                    And [f, fst $ checkNextC tr empty (Historically f)]
                  Triggered f1 f2 ->
                    checkNextC ts cache $
                    And
                      [f2, Or [f1, fst $ checkNextC tr empty (Triggered f1 f2)]]
                  -- Expanded
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

-----------------------------------------------------------------------------
-- | Simplifies a TSL formula
simplify :: Eq c => Formula c -> Formula c
simplify =
  \case
    Not f ->
      case simplify f of
        TTrue -> FFalse
        FFalse -> TTrue
        f' -> Not f'
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
                        g -> g : xs)
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
                        Or g -> g ++ xs
                        FFalse -> xs
                        g -> g : xs)
                   []
                   fs'
    Implies f1 f2 ->
      case (simplify f1, simplify f2) of
        (FFalse, _) -> TTrue
        (TTrue, f) -> f
        (f1', f2') -> Implies f1' f2'
    Equiv f1 f2 ->
      case (simplify f1, simplify f2) of
        (FFalse, f) -> simplify (Not f)
        (f, FFalse) -> simplify (Not f)
        (f, TTrue) -> f
        (TTrue, f) -> f
        (f1', f2') -> Equiv f1' f2'
    f -> f
  where
    isFalse FFalse = True
    isFalse _ = False
    --
    isTrue TTrue = True
    isTrue _ = False
    -- 
    removeDoubles :: Eq a => [a] -> [a]
    removeDoubles [] = []
    removeDoubles (x:xr) =
      if x `elem` xr
        then removeDoubles xr
        else x : removeDoubles xr
