-----------------------------------------------------------------------------
-- |
-- Module      :  Simulator.Core
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple AIGER simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module Simulator.Core
  (
  ) where

-----------------------------------------------------------------------------
import TSL.Aiger (Circuit(..), Input(..), Latch(..), Output(..))

-----------------------------------------------------------------------------
--
-- Defines custom easier to evaluate intermediate tree-shaped circuit
--
data CircuitTree
  = Inp Input
  | InpL Latch
  | AG CircuitTree CircuitTree
  | NG CircuitTree

data NormCircuit =
  NormCircuit
    { inputs :: [Input]
    , outputs :: [Output]
    , latches :: [Latch]
    , outputCir :: Output -> CircuitTree
    , latchCir :: Latch -> CircuitTree
    , inputName :: Input -> String
    , outputName :: Output -> String
    , latchName :: Latch -> String
    }

-----------------------------------------------------------------------------
-- 
-- Transform a circuit into a normalized circuit
--
normalize :: Circuit -> NormCircuit
normalize _ = undefined --TODO

-----------------------------------------------------------------------------
--
-- Defines the (intermediate) state of an aiger circuit. Note that the 
-- assigment function may return undefined
--
type State = Latch -> Bool

type Inputs = Input -> Bool

type Outputs = Output -> Bool

-----------------------------------------------------------------------------
--
-- Evaluation and simulation step of an normalized circuit
--
eval :: CircuitTree -> State -> Inputs -> Bool
eval ct state inpt =
  case ct of
    Inp i -> inpt i
    InpL l -> state l
    AG x y -> eval x state inpt && eval y state inpt
    NG x -> not $ eval x state inpt

simStep :: NormCircuit -> State -> Inputs -> (State, Outputs)
simStep NormCircuit {..} state inpt =
  (\l -> eval (latchCir l) state inpt, \o -> eval (outputCir o) state inpt)
