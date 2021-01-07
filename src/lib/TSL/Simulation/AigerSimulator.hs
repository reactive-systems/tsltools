-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Simulation.AigerSimulator
-- Maintainer  :  Philippe Heim
--
-- A simple AIGER simulator
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

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

import Data.Map as Map (Map, fromList, lookup)

import Control.Exception (assert)

-----------------------------------------------------------------------------
-- | Defines custom easier to evaluate intermediate tree-shaped circuit
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

data CircuitTree
  = Inp Input
  | InpL Latch
  | AG CircuitTree CircuitTree
  | NG CircuitTree
  | CT

-----------------------------------------------------------------------------
-- | Transform a circuit into a normalized circuit (incl. sanitizing)
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

-----------------------------------------------------------------------------
-- | Defines the (intermediate) state of an aiger circuit. Note that the 
-- assigment function may return undefined
type State = Latch -> Bool

type Inputs = Input -> Bool

type Outputs = Output -> Bool

---------------------------------------------------------------------------
-- | Evaluat on and simulation step of an normalized circuit
simStep :: NormCircuit i o -> State -> Inputs -> (State, Outputs)
simStep NormCircuit {..} state inpt =
  let latchMap = functionToMap latches $ \l -> eval (latchCir l) state inpt
      outputMap = functionToMap outputs $ \o -> eval (outputCir o) state inpt
   in (strictLookup latchMap, strictLookup outputMap)
  where
    strictLookup m elem =
      case Map.lookup elem m of
        Just a -> a
        Nothing -> assert False undefined
    --
    functionToMap :: Ord a => [a] -> (a -> b) -> Map a b
    functionToMap keys f = fromList $ fmap (\k -> (k, f k)) keys

eval :: CircuitTree -> State -> Inputs -> Bool
eval ct state inpt =
  case ct of
    Inp i -> inpt i
    InpL l -> state l
    AG x y -> eval x state inpt && eval y state inpt
    NG x -> not $ eval x state inpt
    CT -> True
