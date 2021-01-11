-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Writer.Applicative
-- Maintainer  :  Felix Klein
--
-- Code generation for Applicative FRP.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns
  , MultiWayIf

  #-}

-----------------------------------------------------------------------------

module TSL.Writer.Applicative
  ( implement
  ) where

-----------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import TSL.CFM
  ( Output
  , Wire
  , Term
  , Type(..)
  , CFM(..)
  , constants
  , predicates
  , functions
  , termType
  , prType
  )

import Data.Maybe
  ( mapMaybe
  )

import Data.Set
  ( toList
  , fromList
  )

import TSL.Aiger
  ( Circuit(..)
  , Invertible(..)
  )

import qualified TSL.Aiger as Circuit
  ( Wire(..)
  , inputs
  , outputs
  )

-----------------------------------------------------------------------------

type ModuleName = String
type FunctionName = String

-----------------------------------------------------------------------------

implement
  :: ModuleName
  -> FunctionName
  -> CFM
  -> String

implement mName fName cfm@CFM{..} =
  let
    directInputs =
      filter (not . loopedInput) inputs

    is = Circuit.inputs control
    os = Circuit.outputs control

    ts =
      filter isPredicate (constants cfm)
      ++ predicates cfm
      ++ filter (not . isPredicate) (constants cfm)
      ++ functions cfm
  in
    unlines
      [ replicate 77 '-'
      , "-- |"
      , "-- Module : " ++ mName
      , "--"
      , "-- Applicative Interface for " ++ fName ++ "."
      , "--"
      , replicate 77 '-'
      , ""
      , "{-# LANGUAGE"
      , ""
      , "    Rank2Types"
      , "  , RecordWildCards"
      , "  , DuplicateRecordFields"
      , ""
      , "  #-}"
      , ""
      , replicate 77 '-'
      , ""
      , "module " ++ mName
      , "  ( Input(..)"
      , "  , Output(..)"
      , "  , Functions(..)"
      , "  , InitialState(..)"
      , "  , " ++ fName
      , "  ) where"
      , ""
      , replicate 77 '-'
      , ""
      , "data Input " ++
        if null directInputs then "=" else
          "signal " ++
          prPTypes (map (wireType . inputWire) directInputs) ++
          " ="
      , "  Input"
      , if null directInputs then "" else
          "    { " ++ inputDecl (head directInputs) ++
          concatMap (("\n    , " ++) . inputDecl) (tail directInputs) ++
          "\n    }\n"
      , replicate 77 '-'
      , ""
      , "data Output " ++
        if null outputs then "=" else
          "signal " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs) ++
          " ="
      , "  Output"
      , if null outputs then "" else
          "    { " ++ outputDecl (head outputs) ++
          concatMap (("\n    , " ++) . outputDecl) (tail outputs) ++
          "\n    }\n"
      , replicate 77 '-'
      , ""
      , "data Functions " ++
        if null ts then "=" else
          prPTypes types ++ " ="
      , "  Functions"
      , if null ts then "" else
          "    { " ++ functionDecl (head ts) ++
          concatMap (("\n    , " ++) . functionDecl) (tail ts) ++
          "\n    }\n"
      , replicate 77 '-'
      , ""
      , "data InitialState " ++
        if null outputs then "=" else
          prPTypes (map (wireType . fst . head . outputSwitch) outputs) ++
          " ="
      , "  InitialState"
      , if null outputs then "" else
          "    { " ++ stateDecl (head outputs) ++
          concatMap (("\n    , " ++) . stateDecl) (tail outputs) ++
          "\n    }\n"
      , replicate 77 '-'
      , ""
      , "data ControlIn signal ="
      , "  ControlIn"
      , if null is then "" else
          "    { controlIn" ++ show (head is) ++ " :: signal Bool\n" ++
          concatMap ((++ " :: signal Bool\n") .
                     ("    , controlIn" ++) . show) (tail is) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , "data ControlOut signal ="
      , "  ControlOut"
      , if null is then "" else
          "    { controlOut" ++ show (head os) ++ " :: signal Bool\n" ++
          concatMap ((++ " :: signal Bool\n") .
                     ("    , controlOut" ++) . show) (tail os) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , fName
      , "  :: Applicative signal"
      , "  => (forall poly. poly -> signal poly -> signal poly)"
      , "  -> Functions" ++
        if null ts then "" else " " ++ prPTypes types
      , "  -> InitialState" ++
        if null outputs then "" else " " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs)
      , "  -> Input" ++
        if null directInputs then "" else " signal " ++
          prPTypes (map (wireType . inputWire) directInputs)
      , "  -> " ++
        if null outputs then "Output" else "Output signal " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs)
      , ""
      , fName ++ " cell Functions{..} InitialState{..} Input{..} ="
      , "  let"
      , concatMap prOutputCell outputs
      , concatMap (prTerm' cfm) terms
      , "    ControlOut{..} ="
      , "      controlCircuit cell"
      , "        ControlIn"
      , if null is then "" else
          "          { controlIn0 = " ++
          prWire cfm (controlInputWire $ head is) ++
          concatMap
            (\(n,x) -> "\n          , controlIn" ++ show n ++
                      " = " ++ prWire cfm (controlInputWire x))
            (zip [1 :: Int,2..] $ tail is) ++
          "\n          }\n" ++
        concatMap prSwitch outputs ++ "  in"
      , "    Output"
      , if null outputs then "" else
          "      { " ++ outputName (head outputs) ++ " = " ++
          outputName (head outputs) ++ "Out\n" ++
          concatMap
            (\x -> "      , " ++ outputName x ++
                  " = " ++ outputName x ++ "Out\n")
            (tail outputs) ++
          "      }\n"
      , replicate 77 '-'
      , concatMap (prSwitchImpl cfm) outputs
      ]
      ++
      prCircuitImpl control
      ++
      replicate 77 '-'

  where
    inputDecl i =
      inputName i ++ " :: signal "
      ++ prT (wireType $ inputWire i)

    outputDecl o =
      outputName o ++ " :: signal "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

    functionDecl f =
      termName f ++ " :: " ++ prChain (termType cfm f)

    stateDecl o =
      outputName o ++ " :: "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

    prOutputCell o =
      "    " ++ outputName o ++
      "Cell = cell " ++ outputName o ++
      " " ++ outputName o ++ "Out\n"

    prSwitch o =
      "\n    " ++ outputName o ++ "Out =\n" ++
      "      " ++ outputName o ++ "Switch\n" ++
      concatMap prChoice (outputSwitch o)

    prChoice (w,o) =
      "        " ++ prWire cfm w ++ "\n" ++
      "        controlOut" ++ show o ++ "\n"

    prChain = \case
      []   -> assert False undefined
      [t]  -> prT t
      t:tr -> prT t ++ concatMap ((" -> " ++) . prT) tr

    prPTypes =
      unwords . map prT . toList . fromList . mapMaybe filterP

    prT = \case
      Boolean -> "Bool"
      t       -> prType t

    filterP = \case
      Boolean -> Nothing
      t  -> Just t

-----------------------------------------------------------------------------

prSwitchImpl
  :: CFM -> Output -> String

prSwitchImpl CFM{..} o =
  let
    xs = outputSwitch o
    n = length xs
  in
    unlines
      [ ""
      , outputName o ++ "Switch"
      , "  :: Applicative signal"
      , "  => signal a"
      , "  -> signal Bool"
      , concat (replicate (n - 1)
          "  -> signal a\n  -> signal Bool\n")
        ++ "  -> signal a"
      , ""
      , outputName o ++ "Switch" ++
        concatMap
          (\i -> " s" ++ show i ++ " b" ++ show i)
          [0,1..length (outputSwitch o) - 2] ++
        " s" ++ show (length (outputSwitch o) - 1) ++
        " _ ="
      , "  let ite b s a = " ++
        "(\\b s a -> if b then s else a) <$> b <*> s <*> a"
      , "  in" ++
        concatMap
          (\i -> " ite b" ++ show i ++ " s" ++ show i ++ " $")
          [0,1..length (outputSwitch o) - 3] ++
        " ite b" ++ show (length (outputSwitch o) - 2) ++
        " s" ++ show (length (outputSwitch o) - 2) ++
        " s" ++ show (length (outputSwitch o) - 1)
      , ""
      , replicate 77 '-'
      ]

-----------------------------------------------------------------------------

prCircuitImpl
  :: Circuit ->  String

prCircuitImpl Circuit{..} =
  let
    (ls, ls') = unzip $ map latchDecl latches
    (os, os') = unzip $
      map (\o -> polarized False o 'o' $ outputWire o) outputs
    gs = concatMap gateDecl gates
    ds = concat ls' ++ concat os' ++ gs
  in
    unlines
      [ "controlCircuit"
      , "  :: Applicative signal"
      , "  => (Bool -> signal Bool -> signal Bool)"
      , "  -> ControlIn signal -> ControlOut signal"
      , ""
      , "controlCircuit cell ControlIn{..} ="
      , "  let" ++
        (if null ls then "" else
          concatMap ("\n    " ++) ls) ++
        (if null ds then "" else
          concatMap ("\n    " ++) ds ++ "\n  in")
      , "    ControlOut"
      , if null os then "" else
          "      { controlOut0 = " ++ head os ++
          concatMap
            (\(i,x) -> "\n      , controlOut" ++ show i ++
                      " = " ++ x) (zip [1 :: Int,2..] $ tail os) ++
          "\n      }"
      , let
          hasLatches   = not $ null latches
          hasGates     = not $ null gates
          hasInverters =
              any (isNeg . outputWire) outputs
            || any (isNeg . latchInput) latches
            || any (isNeg . gateInputA) gates
            || any (isNeg . gateInputB) gates
        in
          if hasLatches || hasGates || hasInverters
          then
            "\n  where" ++
            (if hasLatches
             then "\n    _lat_ = cell False"
             else "") ++
            (if hasGates
             then "\n    _and_ x y = (&&) <$> x <*> y"
             else "") ++
            (if hasInverters
             then "\n    _not_ = fmap not"
             else "") ++
            "\n"
          else ""
      ]

  where
    isNeg = \case
      Positive _                  -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                  -> True

    prWire' x
      | Circuit.wire x <= length inputs = "controlIn" ++ show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x

    polarized b i c = \case
      Positive (Circuit.wire -> 0) ->
        (if b then "(pure True)" else "pure True", [])
      Negative (Circuit.wire -> 0) ->
        (if b then "(pure False)" else "pure False", [])
      Positive w ->
        ( prWire' w, [])
      Negative w ->
        ( c : show i
        , [[c] ++ show i ++ " = _not_ " ++ prWire' w]
        )

    latchDecl l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        (vx, d) = polarized True ow 'x' iw
      in
        (prWire' ow ++ " = _lat_ " ++ vx, d)

    gateDecl g =
      let
        iwA = gateInputA g :: Invertible Circuit.Wire
        iwB = gateInputB g :: Invertible Circuit.Wire
        ow = gateOutput g :: Circuit.Wire

        (va, a) = polarized True ow 'a' iwA
        (vb, b) = polarized True ow 'b' iwB
      in
        a ++ b ++ [prWire' ow ++ " = _and_ " ++ va ++ " " ++ vb]

-----------------------------------------------------------------------------

prTerm'
  :: CFM -> Term -> String

prTerm' cfm@CFM{..} t =
  "    " ++ prWire cfm (termOutputWire t) ++ " = " ++
  (case reverse $ termInputWires t of
     []     ->
       ("pure " ++) $ (++ "\n") $
       if
         | termName t == "true"  -> "True"
         | termName t == "false" -> "False"
         | otherwise            -> termName t
     (x:xr) ->
       termName t ++
       " <$> " ++
       prWire cfm x ++
       concatMap ((" <*> " ++) . prWire cfm) xr ++
       "\n")

-----------------------------------------------------------------------------

prWire
  :: CFM -> Wire -> String

prWire CFM{..} w =
  case wireSource w of
    Left i
      | loopedInput i -> inputName i ++ "Cell"
      | otherwise     -> inputName i
    Right t
      | isPredicate t -> 'b' : show w
      | otherwise     -> 'w' : show w

-----------------------------------------------------------------------------
