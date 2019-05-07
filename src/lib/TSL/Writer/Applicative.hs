-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Writer.Applicative
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Code generation for Applicative FRP.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns

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
  unlines
    [ replicate 77 '-'
    , "-- |"
    , "-- Module : " ++ mName
    , "--"
    , "-- Applicative Interface for " ++ fName ++ "."
    , "--"
    , replicate 77 '-'
    , ""
    , "{-# LANGUAGE Rank2Types #-}"
    , ""
    , replicate 77 '-'
    , ""
    , "module " ++ mName
    , "  ( " ++ fName
    , "  ) where"
    , ""
    , replicate 77 '-'
    , ""
    , fName
    , "  :: Applicative signal"
    , "     -- cell implementation"
    , "  => (forall poly. poly -> signal poly -> signal poly)"
    , concatMap prTermType (filter isPredicate (constants cfm)) ++
      concatMap prTermType (predicates cfm) ++
      concatMap prTermType (filter (not . isPredicate) (constants cfm)) ++
      concatMap prTermType (functions cfm) ++
      concatMap prInitType outputs ++
      concatMap
        prInputTypes
        (filter (not . loopedInput) inputs) ++
      prOutputType outputs
    , ""
    , fName
    , "  cell"
    , concatMap
        ((++ "\n") . ("  p_" ++) . termName)
        (filter isPredicate $ constants cfm)
      ++
      concatMap
        ((++ "\n") . ("  p_" ++) . termName)
        (predicates cfm)
      ++
      concatMap
        ((++ "\n") . ("  f_" ++) . termName)
        (filter (not . isPredicate) $ constants cfm)
      ++
      concatMap
        ((++ "\n") . ("  f_" ++) . termName)
        (functions cfm)
      ++
      concatMap
        ((++ "\n") . ("  i_" ++) . outputName)
        outputs
      ++
      concatMap
        ((++ "\n") . ("  s_" ++) . inputName)
        (filter (not . loopedInput) inputs)
    , "  ="
    , ""
    , "  let"
    , concatMap prOutputCell outputs
    , concatMap (prTerm' cfm) terms
    , "    " ++ prTuple (map (("cout" ++) . show) os) ++ " ="
    , "      controlCircuit"
    , "        cell"
    , concatMap
        ((++ "\n") . ("        " ++) . prWire cfm . controlInputWire)
        is
    , concatMap prSwitch outputs ++ "  in"
    , "    " ++
      prTuple (map (("o_" ++) . outputName) outputs)
    , ""
    , replicate 77 '-'
    , concatMap (prSwitchImpl cfm) outputs
    ]
    ++
    prCircuitImpl control
    ++
    replicate 77 '-'

  where
    is = Circuit.inputs control
    os = Circuit.outputs control

    prOutputCell o =
      "    c_" ++ outputName o ++
      " = cell i_" ++ outputName o ++
      " o_" ++ outputName o ++ "\n"

    prOutputType = \case
      []     ->
        "     -- no output\n" ++
        "  -> signal ()"
      [x]    ->
        "     -- " ++ outputName x ++ " (output)\n" ++
        "  -> signal " ++ prResultType x
      (x:xr) ->
        "     -- outputs\n" ++
        "  -> ( -- " ++ outputName x ++ "\n" ++
        "       signal " ++ prResultType x ++ "\n" ++
        concatMap prO xr ++
        "     )"

    prO o =
      "     , -- " ++ outputName o ++ "\n" ++
      "       signal " ++ prResultType o ++ "\n"

    prInputTypes i =
      "     -- " ++ inputName i ++ " (input)\n" ++
      "  -> signal " ++ prT (wireType $ inputWire i) ++ "\n"

    prInitType o =
      "     -- initial value: " ++
      outputName o ++ "\n" ++
      "  -> " ++ prResultType o ++ "\n"

    prResultType =
      prT . wireType . fst . head . outputSwitch

    prSwitch o =
      "    o_" ++ outputName o ++ " =\n" ++
      "      " ++ outputName o ++ "Switch\n" ++
      concatMap prChoice (outputSwitch o)

    prChoice (w,o) =
      "        " ++ prWire cfm w ++ "\n" ++
      "        cout" ++ show o ++ "\n"

    prTermType t =
      "     -- " ++ termName t ++ "\n" ++
      "  -> " ++ prChain (termType cfm t) ++ "\n"

    prChain = \case
      []   -> assert False undefined
      [t]  -> prT t
      t:tr -> "(" ++ prT t ++ concatMap ((" -> " ++) . prT) tr ++ ")"

    prT = \case
      Boolean -> "Bool"
      t       -> prType t

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
  unlines
    [ "controlCircuit"
    , "  :: Applicative signal"
    , "     -- cell implementation"
    , "  => (Bool -> signal Bool -> signal Bool)"
    , "     -- inputs"
    , unlines
        ( replicate
            (length inputs)
            "  -> signal Bool"
        ) ++
      "     -- outputs"
    , "  -> ( signal Bool"
    , unlines
        ( replicate
            (length outputs - 1)
            "     , signal Bool"
        ) ++
      "     )"
    , ""
    , "controlCircuit cell" ++
      concatMap ((" cin" ++) . show) inputs ++
      " ="
    , "  let"
    , concatMap prLatch latches
      ++ concatMap prGate gates
      ++ prOutputs
    , ""
    , "  where"
    , "    _lat_ = cell False"
    , "    _and_ x y = (&&) <$> x <*> y"
    , "    _not_ = fmap not"
    , ""
    ]

  where
    prWire' x
      | Circuit.wire x <= length inputs = "cin" ++ show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x

    polarized i c = \case
      Positive (Circuit.wire -> 0) ->
        ("(pure True)", "")
      Negative (Circuit.wire -> 0) ->
        ("(pure False)", "")
      Positive w ->
        ( prWire' w, "")
      Negative w ->
        ( c : show i
        , "    " ++ [c] ++ show i
          ++ " = _not_ " ++ prWire' w ++ "\n"
        )

    prLatch l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        (vx, x) = polarized ow 'x' iw
      in
        x ++ "    " ++ prWire' ow ++ " = _lat_ " ++ vx ++ "\n"

    prGate g =
      let
        iwA = gateInputA g :: Invertible Circuit.Wire
        iwB = gateInputB g :: Invertible Circuit.Wire
        ow = gateOutput g :: Circuit.Wire

        (va, a) = polarized ow 'a' iwA
        (vb, b) = polarized ow 'b' iwB
      in
        a ++ b ++ "    " ++ prWire' ow
        ++ " = _and_ " ++ va ++ " " ++ vb ++ "\n"

    prOutputs =
      let
        (os, xs) =
          unzip $ map (\o -> polarized o 'o' $ outputWire o) outputs
      in
        concat xs ++ "  in\n    " ++ prTuple os

-----------------------------------------------------------------------------

prTuple
  :: [String] -> String

prTuple = \case
  []   -> "()"
  [x]  -> x
  x:xr -> "(" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ")"

-----------------------------------------------------------------------------

prTerm'
  :: CFM -> Term -> String

prTerm' cfm@CFM{..} t =
  "    " ++ prWire cfm (termOutputWire t) ++ " = " ++
  (case reverse $ termInputWires t of
     []     ->
       "pure " ++
       (if isPredicate t then "p_" else "f_") ++
       termName t ++
       "\n"
     (x:xr) ->
       (if isPredicate t then "p_" else "f_") ++
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
      | loopedInput i -> "c_" ++ inputName i
      | otherwise     -> "s_" ++ inputName i
    Right t
      | isPredicate t -> 'b' : show w
      | otherwise     -> 'w' : show w

-----------------------------------------------------------------------------
