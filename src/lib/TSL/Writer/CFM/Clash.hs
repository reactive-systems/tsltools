-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Writer.CFM.Clash
-- Maintainer  :  Felix Klein
--
-- Clash code generation.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

-----------------------------------------------------------------------------

module TSL.Writer.CFM.Clash
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
      , "-- Clash Interface for " ++ fName ++ "."
      , "--"
      , replicate 77 '-'
      , ""
      , "{-# LANGUAGE"
      , ""
      , "    RecordWildCards"
      , "  , DuplicateRecordFields"
      , ""
      , "  #-}"
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
      , "import Clash.Prelude"
      , ""
      , replicate 77 '-'
      , ""
      , "data Input " ++
        if null directInputs then "=" else
          "domain " ++
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
          "domain " ++
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
      , "data ControlIn domain ="
      , "  ControlIn"
      , if null is then "" else
          "    { controlIn" ++ show (head is) ++ " :: Signal domain Bit\n" ++
          concatMap ((++ " :: Signal domain Bit\n") .
                     ("    , controlIn" ++) . show) (tail is) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , "data ControlOut domain ="
      , "  ControlOut"
      , if null is then "" else
          "    { controlOut" ++ show (head os) ++ " :: Signal domain Bit\n" ++
          concatMap ((++ " :: Signal domain Bit\n") .
                     ("    , controlOut" ++) . show) (tail os) ++
          "    }\n"
      , replicate 77 '-'
      , ""
      , fName
      , "  :: HiddenClockReset domain gated synchronous"
      , "  => Functions" ++
        if null ts then "" else " " ++ prPTypes types
      , "  -> InitialState" ++
        if null outputs then "" else " " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs)
      , "  -> Input" ++
        if null directInputs then "" else " domain " ++
          prPTypes (map (wireType . inputWire) directInputs)
      , "  -> " ++
        if null outputs then "Output" else "Output domain " ++
          prPTypes (map (wireType . fst . head . outputSwitch) outputs)
      , ""
      , fName  ++ " Functions{..} InitialState{..} Input{..} ="
      , "  let"
      , concatMap prOutputCell outputs
      , concatMap (prTerm' cfm) terms
      , "    ControlOut{..} ="
      , "      controlCircuit"
      , "        ControlIn"
      , if null is then "" else
          "          { controlIn0 = " ++
          prWire' (controlInputWire $ head is) ++
          concatMap
            (\(n,x) -> "\n          , controlIn" ++ show n ++
                      " = " ++ prWire' (controlInputWire x))
            (zip [1 :: Int,2..] $ tail is) ++
          "\n          }\n"

      , concatMap prSwitch outputs ++ "  in"
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
    prWire' w = case wireSource w of
      Left _  -> "((! 0) . pack) <$> " ++ prWire cfm w
      Right _ -> prWire cfm  w

    inputDecl i =
      inputName i ++ " :: Signal domain "
      ++ prT (wireType $ inputWire i)

    outputDecl o =
      outputName o ++ " :: Signal domain "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

    functionDecl f =
      termName f ++ " :: " ++ prChain (termType cfm f)

    stateDecl o =
      outputName o ++ " :: "
      ++ prT (wireType $ fst $ head $ outputSwitch o)

    prPTypes =
      unwords . map prT . toList . fromList . mapMaybe filterP

    prOutputCell o =
      "    " ++ outputName o ++
      "Cell = register " ++ outputName o ++
      " " ++ outputName o ++ "Out\n"

    prChain = \case
      []   -> assert False undefined
      [t]  -> prT t
      t:tr -> prT t ++ concatMap ((" -> " ++) . prT) tr

    prT = \case
      Boolean -> "Bool"
      t       -> prType t

    filterP = \case
      Boolean -> Nothing
      t       -> Just t

    prSwitch o =
      "    " ++ outputName o ++ "Out =\n" ++
      "      " ++ outputName o ++ "Switch\n" ++
      (case outputSwitch o of
         []               ->
           "        $ pure $ pack () \n"
         [(w,x)]          ->
           "        (fmap pack controlOut" ++ show x ++ ")\n" ++
           "        " ++ prWire cfm w ++ "\n"
         [(w,x), (w',x')] ->
           "        ( liftA2 (++#) (fmap pack controlOut" ++
           show x ++ ")\n" ++
           "        $ fmap pack controlOut" ++ show x' ++ "\n" ++
           "        )\n" ++
           "        ( liftA2 (:>) " ++ prWire cfm w ++ "\n" ++
           "        $ liftA2 (:>) " ++ prWire cfm w' ++ "\n" ++
           "        $ pure Nil\n" ++
           "        )\n"
         (w,x):xr ->
           "        ( liftA2 (++#) (fmap pack controlOut" ++
           show x ++ ")\n" ++
           concatMap prArg (init xr) ++
           "        $ fmap pack controlOut" ++
           show (snd $ last xr) ++ "\n" ++
           "        )\n" ++
           "        ( liftA2 (:>) " ++ prWire cfm w ++ "\n" ++
           concatMap prChoice xr ++
           "        $ pure Nil\n" ++
           "        )\n")

    prArg (_,x) =
      "        $ liftA2 (++#) (fmap pack controlOut" ++ show x ++ ")\n"

    prChoice (w,_) =
      "        $ liftA2 (:>) " ++ prWire cfm  w ++ "\n"

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
      , "  :: HiddenClockReset domain gated synchronous"
      , "  => Signal domain (BitVector " ++ show n ++ ")"
      , "  -> Signal domain (Vec " ++ show n ++ " a)"
      , "  -> Signal domain a"
      , ""
      , outputName o ++ "Switch = liftA2 select"
      , "  where"
      , "    select bs vs"
      , concatMap
          (\i -> "      | bs ! " ++
                show i ++ " == high = vs !! " ++
                show (n - 1 - i) ++ "\n"
          ) [0,1..n-2] ++
        "      | otherwise      = vs !! 0\n"
      , replicate 77 '-'
      ]

-----------------------------------------------------------------------------

prCircuitImpl
  :: Circuit -> String

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
      , " :: HiddenClockReset domain gated synchronous"
      , " => ControlIn domain -> ControlOut domain"
      , ""
      , "controlCircuit ControlIn{..} = "
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
             then "\n    _lat_ = register low"
             else "") ++
            (if hasGates
             then "\n    _and_ = liftA2 (.&.)"
             else "") ++
            (if hasInverters
             then "\n    _not_ = fmap complement"
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
      | Circuit.wire x <= length inputs = "controlIn" ++
                                          show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x

    polarized b i c = \case
      Positive (Circuit.wire -> 0) ->
        (if b then "(pure high)" else "pure high", [])
      Negative (Circuit.wire -> 0) ->
        (if b then "(pure low)" else "pure low", [])
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
         | otherwise             -> termName t
     (x:xr) ->
       (if isPredicate t
        then if null xr
             then "pack . "
             else "((! 0) . pack) <$> ("
        else "") ++
       termName t ++
       " <$> " ++
       prWire cfm x ++
       concatMap ((" <*> " ++) . prWire cfm) xr ++
       (if isPredicate t && not (null xr) then ")" else "") ++
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
