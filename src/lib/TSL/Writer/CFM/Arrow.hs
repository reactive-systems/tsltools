-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Writer.Arrow
-- Maintainer  :  Felix Klein
--
-- Code generation for Arrowized FRP.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

-----------------------------------------------------------------------------

module TSL.Writer.CFM.Arrow
  ( implement
  ) where

-----------------------------------------------------------------------------

import Control.Exception (assert)

import TSL.CFM
  ( CFM(..)
  , Output
  , Term
  , Type(..)
  , Wire
  , constants
  , functions
  , prType
  , predicates
  , termType
  )

import Data.Maybe (mapMaybe)

import Data.Set (fromList, toList)

import TSL.Aiger (Circuit(..), Invertible(..))

import qualified TSL.Aiger as Circuit (Wire(..), inputs, outputs)

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
  let ?bounds = cfm in
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
      , "-- Arrow Interface for " ++ fName ++ "."
      , "--"
      , replicate 77 '-'
      , "{-# LANGUAGE"
      , ""
      , "    Arrows"
      , "  , Rank2Types"
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
      , "import Control.Arrow"
      , "  ( Arrow"
      , "  , ArrowLoop"
      , "  , returnA"
      , "  , arr"
      , "  , (<<<)"
      , "  )"
      , ""
      , replicate 77 '-'
      , ""
      , "data Input " ++
        if null directInputs then "=" else
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
      , "data ControlIn ="
      , "  ControlIn"
      , if null is then "" else
          "    { controlIn" ++ show (head is) ++ " :: Bool\n" ++
          concatMap ((++ " :: Bool\n") .
                     ("    , controlIn" ++) . show) (tail is) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , "data ControlOut ="
      , "  ControlOut"
      , if null is then "" else
          "    { controlOut" ++ show (head os) ++ " :: Bool\n" ++
          concatMap ((++ " :: Bool\n") .
                     ("    , controlOut" ++) . show) (tail os) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , fName
      , "  :: (Arrow signal, ArrowLoop signal)"
      , "  => (forall poly. poly -> signal poly poly)"
      , "  -> Functions" ++
        if null ts then "" else " " ++ prPTypes types
      , "  -> InitialState" ++
        if null outputs then "" else " " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs)
      , "  -> signal"
      , "       (Input" ++
        if null directInputs then ")" else
          " " ++
          prPTypes (map (wireType . inputWire) directInputs) ++
          ")"
      , "       (Output" ++
        if null outputs then ")" else
          " " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs) ++
          ")"
      , ""
      , fName ++ " cell Functions{..} InitialState{..}" ++
        " = proc Input{..} -> do"
      , "  rec"
      , concatMap prOutputCell outputs
      , concatMap (prTerm' cfm) terms
      , "    ControlOut{..} <-"
      , "      controlCircuit cell -<"
      , "        ControlIn"
      , if null is then "" else
          "          { controlIn0 = " ++
          prWire cfm (controlInputWire $ head is) ++
          concatMap
            (\(n,x) -> "\n          , controlIn" ++ show n ++
                      " = " ++ prWire cfm (controlInputWire x))
            (zip [1 :: Int,2..] $ tail is) ++
          "\n          }\n"
      , concatMap prSwitch outputs ++ "  returnA -<"
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
    prOutputCell o =
      "    " ++ outputName o ++
      "Cell <- cell " ++ outputName o ++
      " -< " ++ outputName o ++ "Out\n"

    inputDecl i =
      inputName i ++ " :: "
      ++ prT (wireType $ inputWire i)

    outputDecl o =
      outputName o ++ " :: "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

    functionDecl f =
      termName f ++ " :: " ++ prChain (termType cfm f)

    stateDecl o =
      outputName o ++ " :: "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

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
      t       -> Just t

    prSwitch o =
      indent 4 (outputName o) ++ "Out <-\n" ++
      indent 6 (outputName o) ++ "Switch -<\n" ++
      prMultiLineTuple 8
        (map (\(w,x) -> "(" ++ prWire cfm w ++
                       ", controlOut" ++ show x ++ ")")
           $ outputSwitch o) ++ "\n\n"

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
      , "  :: Arrow signal"
      , "  => signal"
      , prMultiLineTuple 7 (replicate n "(a, Bool)") ++ " a"
      , ""
      , outputName o ++ "Switch ="
      , "  proc"
      , prMultiLineTuple 4
          ( map
              (\i -> "(s" ++ show i ++ ", b" ++ show i ++ ")")
              [0,1..n-2]
            ++
            ["(s" ++ show (n-1) ++ ", _)"]
          ) ++ " ->"
      , "  do"
      , concatMap
          (\i ->
            indent 4 "r" ++ show i ++
            " <- arr ite -< (b" ++ show i ++
            ", s" ++ show i ++ ", " ++
            (if n == i + 2 then "s" else "r") ++
            show (i+1) ++ ")\n"
          )
          [n-2,n-3..0] ++ indent 4 "returnA -< r0"
      , ""
      , "  where"
      , "    ite (b, t, e) = if b then t else e"
      , ""
      , replicate 77 '-'
      ]

-----------------------------------------------------------------------------

prCircuitImpl
  :: Circuit -> String

prCircuitImpl Circuit{..} =
  let
    (os, xs) = unzip $ map (\o -> polarized o 'o' $ outputWire o) outputs
  in
    unlines
      [ "controlCircuit"
      , "  :: (Arrow signal, ArrowLoop signal)"
      , "  => (Bool -> signal Bool Bool)"
      , "  -> signal ControlIn ControlOut"
      , ""
      , "controlCircuit cell = proc ControlIn{..} -> do"
      , "  rec"
      , concatMap prLatch latches
      , indent 4 "let"
      , concatMap prGate gates ++ concat xs ++ "\n  returnA -<"
      , "    ControlOut"
      , if null os then "" else
          "      { controlOut0 = " ++ head os ++
          concatMap
            (\(i,x) -> "\n      , controlOut" ++ show i ++
                      " = " ++ x) (zip [1 :: Int,2..] $ tail os) ++
          "\n      }"
      , let
          hasLatches   = not $ null latches
          hasInverters =
              any (isNeg . outputWire) outputs
            || any (isNeg . latchInput) latches
            || any (isNeg . gateInputA) gates
            || any (isNeg . gateInputB) gates
        in
          if hasLatches || hasInverters
          then
            "\n  where" ++
            (if hasLatches
             then "\n    _lat_ = cell False"
             else "") ++
            (if hasInverters
             then "\n    _not_ = arr not"
             else "") ++
            "\n"
          else ""
      ]

  where
    isNeg = \case
      Positive _                   -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                   -> True

    prWire' x
      | Circuit.wire x <= length inputs = "controlIn" ++ show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x

    prLatch l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        polarization = case iw of
          Negative w -> "<<< _not_ -< " ++ prWire' w
          Positive w -> "-< " ++ prWire' w
      in
        indent 4 (prWire' ow) ++
        " <- _lat_ " ++ polarization ++ "\n"

    prGate g =
      let
        iwA = gateInputA g :: Invertible Circuit.Wire
        iwB = gateInputB g :: Invertible Circuit.Wire
        ow = gateOutput g :: Circuit.Wire
      in
        indent 6 (prWire' ow) ++ " = " ++
        poled iwA ++ " && " ++ poled iwB ++ "\n"

    poled = \case
      Positive (Circuit.wire -> 0) -> "True"
      Negative (Circuit.wire -> 0) -> "False"
      Positive w                   -> prWire' w
      Negative w                   -> "not " ++ prWire' w

    polarized i c = \case
      Positive (Circuit.wire -> 0) ->
        ( "True", "")
      Negative (Circuit.wire -> 0) ->
        ( "False", "")
      Positive w ->
        ( prWire' w, "")
      Negative w ->
        ( c : show i
        , indent 6 (c : show i) ++
          " = not " ++ prWire' w ++ "\n"
        )

-----------------------------------------------------------------------------

prMultiLineTuple
  :: Int -> [String] -> String

prMultiLineTuple n = \case
  []   -> indent n "()"
  [x]  -> indent n x
  x:xr ->
    indent n ("( " ++ x ++ "\n") ++
    concatMap (indent n . (", " ++) . (++ "\n")) xr ++
    indent n ")"

-----------------------------------------------------------------------------

indent
  :: Int -> String -> String

indent n x =
  iterate (' ':) x !! n

-----------------------------------------------------------------------------

prTerm'
  :: CFM -> Term -> String

prTerm' cfm@CFM{..} t =
  "    " ++ prWire cfm (termOutputWire t) ++ " <- " ++
  (case reverse $ termInputWires t of
     []     ->
       ("arr (const " ++) $ (++ ") -< ()\n") $
       if
         | termName t == "true"  -> "True"
         | termName t == "false" -> "False"
         | isPredicate t         -> termName t
         | otherwise             -> termName t
     (x:xr) ->
       "arr " ++
       (iterate (("(uncurry " ++) . (++ ")"))
         (termName t) !! length xr) ++ " -< " ++
       prT (prWire cfm x) xr ++ "\n")

  where
    prT s = \case
      []   -> s
      x:xr -> prT ("(" ++ s ++ ", " ++ prWire cfm x ++ ")") xr

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
