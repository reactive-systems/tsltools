-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.AigerSimulator
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A simple AIGER simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module TSL.Simulation.AigerSimulator
  ( NormCircuit(inputs, outputs, latches, inputName, outputName)
  , State
  , Input
  , Output
  , normalize
  , simStep
  ) where

-----------------------------------------------------------------------------
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

import Control.Exception (assert)

-----------------------------------------------------------------------------
--
-- Defines custom easier to evaluate intermediate tree-shaped circuit
--
data CircuitTree
  = Inp Input
  | InpL Latch
  | AG CircuitTree CircuitTree
  | NG CircuitTree
  | CT

data NormCircuit i o =
  NormCircuit
    { inputs :: [Input]
    , outputs :: [Output]
    , latches :: [Latch]
    , outputCir :: Output -> CircuitTree
    , latchCir :: Latch -> CircuitTree
    , inputName :: Input -> i
    , outputName :: Output -> o
    }

-----------------------------------------------------------------------------
-- 
-- Transform a circuit into a normalized circuit
-- ASSUMPTIONS: 
-- - The aiger circuit contains no logic loops
--
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
        , outputCir = \o -> iwire2ct $ Aiger.outputWire aig o
        , latchCir = \l -> iwire2ct $ Aiger.latchInput aig l
        , inputName = lookup $ rights $ renamedInputs
        , outputName = lookup $ rights $ renamedOutputs
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
            Nothing ->
              case isLatchOutput w of
                Just l -> InpL l
                Nothing -> CT
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
    CT -> True

simStep :: NormCircuit i o -> State -> Inputs -> (State, Outputs)
simStep NormCircuit {..} state inpt =
  (\l -> eval (latchCir l) state inpt, \o -> eval (outputCir o) state inpt)
