-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.AigerSimulator
-- Description :  A simple AIGER simulator
-- Maintainer  :  Philippe Heim
--
-- This module provides functionalities to evaluate AIGER circuits on
-- sequences of inputs, i.e. functionalities to simulate AIGER circuits.
--
-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

-------------------------------------------------------------------------------
module TSL.Simulation.AigerSimulator
  ( NormCircuit(inputs, outputs, latches, inputName, outputName)
  , State
  , Input
  , Output
  , normalize
  , simStep
  ) where

-------------------------------------------------------------------------------
import TSL.Aiger as Aiger
  ( Circuit(..)
  , Gate
  , Input
  , Invertible(..)
  , Latch
  , Output
  , Wire
  )

import Data.Either (lefts, rights)
import Data.List (find)

import Data.Map as Map (Map, fromList, lookup)

import Control.Exception (assert)

-----------------------------------------------------------------------------
-- | 'NormCircuit' defines an intermediate tree-shaped representation of the
-- AIGER circuit, that can be evaluated in an recursive manner. Hereby a
-- 'CircuitTree' is the evaluation-tree (of an output or latch input). The 
-- leafs of such an evaluation-tree are consequently latches or inputs of the
-- circuit.

data NormCircuit i o =
  NormCircuit
    { 
    -- | The inputs of the circuit
      inputs :: [Input]
    -- | The outputs of the circuit
    , outputs :: [Output]
    -- | The latches of the circuit
    , latches :: [Latch]
    -- | Assigns each output its evaluation tree
    , outputCir :: Output -> CircuitTree
    -- | Assigns each latch its evaluation tree
    , latchCir :: Latch -> CircuitTree
    -- | A labeling for the inputs
    , inputName :: Input -> i
    -- | A labeling for the outputs
    , outputName :: Output -> o
    }

data CircuitTree
  -- | Input (leaf) of the evaluation-tree in form of an input
  = Inp Input
  -- | Input (leaf) of the evaluation-tree in form of a latch
  | InpL Latch
  -- | AND-Gate of the evaluation-tree
  | AG CircuitTree CircuitTree
  -- | NOT-Gate of the evaluation-tree
  | NG CircuitTree
  -- | Constant-TRUE value
  | CT

-------------------------------------------------------------------------------
-- | 'normalize' transforms a 'Circuit' into a normalized 'NormCircuit'.
-- ASSUMPTIONS:
-- - The aiger circuit contains no logic loops

normalize ::
     (String -> Either err i)
  -> (String -> Either err o)
  -> Circuit
  -> Either err (NormCircuit i o)
normalize renameInput renameOutput aig =
  case (lefts renamedInputs, lefts renamedOutputs) of
    (e:_, _) -> Left e
    ([], e:_) -> Left e
    ([], []) ->
      Right $
      NormCircuit
        { inputs = Aiger.inputs aig
        , outputs = Aiger.outputs aig
        , latches = Aiger.latches aig
        , outputCir = iwire2ct . Aiger.outputWire aig 
        , latchCir = iwire2ct . Aiger.latchInput aig
        , inputName = lookup $ rights renamedInputs
        , outputName = lookup $ rights renamedOutputs
        }
  where
    renamedInputs = rename (Aiger.inputs aig) (Aiger.inputName aig) renameInput
    renamedOutputs =
      rename (Aiger.outputs aig) (Aiger.outputName aig) renameOutput
    --
    rename :: [a] -> (a -> b) -> (b -> Either err c) -> [Either err (a, c)]
    rename xs toStr rnm =
      map
        (\x ->
           case rnm (toStr x) of
             Left err -> Left err
             Right v -> Right (x, v))
        xs
    --
    lookup :: Eq a => [(a, b)] -> a -> b
    lookup [] _ = assert False undefined
    lookup ((k, v):xr) a =
      if k == a
        then v
        else lookup xr a
    --
    iwire2ct :: Invertible Wire -> CircuitTree
    iwire2ct =
      \case
        Positive w -> wire2ct w
        Negative w -> NG $ wire2ct w
    --
    wire2ct :: Wire -> CircuitTree
    wire2ct w =
      case isGateOutput w of
        Just g ->
          AG
            (iwire2ct $ Aiger.gateInputA aig g)
            (iwire2ct $ Aiger.gateInputB aig g)
        Nothing ->
          case isInputWire w of
            Just i -> Inp i
            Nothing -> maybe CT InpL (isLatchOutput w)
    --
    isInputWire :: Wire -> Maybe Input
    isInputWire w = find (\i -> w == Aiger.inputWire aig i) (Aiger.inputs aig)
    --
    isGateOutput :: Wire -> Maybe Gate
    isGateOutput w = find (\g -> w == Aiger.gateOutput aig g) (Aiger.gates aig)
    -- 
    isLatchOutput :: Wire -> Maybe Latch
    isLatchOutput w =
      find (\l -> w == Aiger.latchOutput aig l) (Aiger.latches aig)

-------------------------------------------------------------------------------
-- | Aliases for intermediate states and in- and output assignments

type State = Latch -> Bool

type Inputs = Input -> Bool

type Outputs = Output -> Bool

-------------------------------------------------------------------------------
-- | 'simStep' computes given a state and an input, the next state and the 
-- output of a 'NormCircuit'

simStep :: NormCircuit i o -> State -> Inputs -> (State, Outputs)
simStep NormCircuit {..} state inpt =
  let latchMap = functionToMap latches $ \l -> eval (latchCir l) state inpt
      outputMap = functionToMap outputs $ \o -> eval (outputCir o) state inpt
   in (strictLookup latchMap, strictLookup outputMap)
  where
    -- The following is needed to avoid that the mapping functions are 
    -- evaluated in a lazy manner each step
    strictLookup m elem =
      case Map.lookup elem m of
        Just a -> a
        Nothing -> assert False undefined
    --
    functionToMap :: Ord a => [a] -> (a -> b) -> Map a b
    functionToMap keys f = fromList $ fmap (\k -> (k, f k)) keys

-------------------------------------------------------------------------------
-- | 'eval' computes the result of a 'CircuitTree' when evaluated on some
-- latch and input assignment
eval :: CircuitTree -> State -> Inputs -> Bool
eval ct state inpt =
  case ct of
    Inp i -> inpt i
    InpL l -> state l
    AG x y -> eval x state inpt && eval y state inpt
    NG x -> not $ eval x state inpt
    CT -> True
