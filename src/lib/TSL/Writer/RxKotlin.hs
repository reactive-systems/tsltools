{-# LANGUAGE

    RecordWildCards
  , ImplicitParams
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns
  , MultiWayIf

  #-}

-----------------------------------------------------------------------------

module TSL.Writer.RxKotlin
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
  let ?bounds = cfm in
  unlines
    [ replicate 77 '-'
    , "// |"
    , "// Module : " ++ mName
    , "//"
    , "// Arrow Interface for " ++ fName ++ "."
    , "//"
    , replicate 77 '-'
    , ""
    , "{-# LANGUAGE Rank2Types, Arrows #-}"
    , ""
    , replicate 77 '-'
    , ""
    , "module " ++ mName
    , "  ( " ++ fName
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
    , fName
    , "  :: (Arrow signal, ArrowLoop signal)"
    , "     // cell implementation"
    , "  => (forall poly. poly -> signal poly poly)"
    , concatMap prTermType (filter isPredicate (constants cfm)) ++
      concatMap prTermType (predicates cfm) ++
      concatMap prTermType (filter (not . isPredicate) (constants cfm)) ++
      concatMap prTermType (functions cfm) ++
      concatMap prInitType outputs ++
      "     // arrow"
    , "  -> signal"
    , prInputType (filter (not . loopedInput) inputs)
    , prOutputType outputs
    , ""
    , fName
    , "  cell"
    , concatMap
        ((++ "\n") . ("  p_" ++) . termName)
        (filter isPredicate (constants cfm))
      ++
      concatMap
        ((++ "\n") . ("  p_" ++) . termName)
        (predicates cfm)
      ++
      concatMap
        ((++ "\n") . ("  f_" ++) . termName)
        (filter (not . isPredicate) (constants cfm))
      ++
      concatMap
        ((++ "\n") . ("  f_" ++) . termName)
        (functions cfm)
      ++
      concatMap
        ((++ "\n") . ("  i_" ++) . outputName)
        outputs
    , "  ="
    , ""
    , "  proc"
    , prMultiLineTuple 4
        ( map (("s_" ++) . inputName)
        $ filter (not . loopedInput) inputs
        ) ++ " ->"
    , "  do"
    , "    rec"
    , concatMap prOutputCell outputs
    , concatMap (prTerm' cfm) terms
    , "      " ++ prTuple (map (("cout" ++) . show) os) ++ " <-"
    , "        controlCircuit cell -<"
    , prMultiLineTuple 10 (map (prWire cfm . controlInputWire) is)
    , ""
    , concatMap prSwitch outputs ++
      "    returnA -< " ++
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
    prOutputCell o =
      "      c_" ++ outputName o ++
      " <- cell i_" ++ outputName o ++
      " -< o_" ++ outputName o ++ "\n"

    prOutputType = \case
      []   ->
        "       // no output\n" ++
        "       ()"
      [x]  ->
        "       // " ++ outputName x ++ " (output)\n" ++
        "       " ++ prResultType x
      x:xr ->
        "       ( // " ++ outputName x ++ " (output)\n" ++
        "         " ++ prResultType x ++ "\n" ++
        concatMap prO xr ++
        "       )"

    prO o =
      "       , // " ++ outputName o ++ " (output)\n" ++
      "         " ++ prResultType o ++ "\n"

    prInputType = \case
      []   ->
        "       // no input\n" ++
        "       ()"
      [x]  ->
        "       // " ++ inputName x ++ " (input)\n" ++
        "        " ++ prT (wireType $ inputWire x)
      x:xr ->
        "       ( // " ++ inputName x ++ " (input)\n" ++
        "         " ++ prT (wireType $ inputWire x) ++ "\n" ++
        concatMap prI xr ++
        "       )"

    prI i =
      "       , // " ++ inputName i ++ " (input)\n" ++
      "         " ++ prT (wireType $ inputWire i) ++ "\n"

    prInitType o =
      "     // initial value: " ++ outputName o ++ "\n" ++
      "  -> " ++ prResultType o ++ "\n"

    prResultType =
      prT . wireType . fst . head . outputSwitch

    prChain = \case
      []   -> assert False undefined
      [t]  -> prT t
      t:tr -> "(" ++ prT t ++ concatMap ((" -> " ++) . prT) tr ++ ")"

    prT = \case
      Boolean -> "Bool"
      t       -> prType t

    prTermType t =
      "     // " ++ termName t ++ "\n" ++
      "  -> " ++ prChain (termType cfm t) ++ "\n"

    is = Circuit.inputs control
    os = Circuit.outputs control

    prSwitch o =
      indent 6 ("o_" ++ outputName o) ++ " <-\n" ++
      indent 8 (outputName o) ++ "Switch -<\n" ++
      prMultiLineTuple 10
        (map (\(w,x) -> "(" ++ prWire cfm w ++ ", cout" ++ show x ++ ")")
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
  unlines
    [ "controlCircuit"
    , "  :: (Arrow signal, ArrowLoop signal)"
    , "     -- cell implementation"
    , "  => (Bool -> signal Bool Bool)"
    , "     -- circuit"
    , "  -> signal"
    , prMultiLineTuple 7
        $ replicate (length inputs) "Bool"
    , prMultiLineTuple 7
        $ replicate (length outputs) "Bool"
    , ""
    , "controlCircuit cell ="
    , "  proc"
    , prMultiLineTuple 4
        (map (("cin" ++) . show) inputs) ++ " ->"
    , "  do"
    , "    rec"
    , concatMap prLatch latches
    , indent 6 "let"
    , concatMap prGate gates ++ prOutputs
    , let
        hasLatches   = not $ null $ latches
        hasInverters =
            any isNeg (map outputWire outputs)
          || any isNeg (map latchInput latches)
          || any isNeg (map gateInputA gates)
          || any isNeg (map gateInputB gates)
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
      Positive _                  -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                  -> True

    prWire' x
      | Circuit.wire x <= length inputs = "cin" ++ show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x

    prLatch l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        polarization = case iw of
          Negative w -> "<<< _not_ -< " ++ prWire' w
          Positive w -> "-< " ++ prWire' w
      in
        indent 6 (prWire' ow) ++
        " <- _lat_ " ++ polarization ++ "\n"

    prGate g =
      let
        iwA = gateInputA g :: Invertible Circuit.Wire
        iwB = gateInputB g :: Invertible Circuit.Wire
        ow = gateOutput g :: Circuit.Wire
      in
        indent 8 (prWire' ow) ++ " = " ++
        poled iwA ++ " && " ++ poled iwB ++ "\n"

    poled = \case
      Positive (Circuit.wire -> 0) -> "True"
      Negative (Circuit.wire -> 0) -> "False"
      Positive w -> prWire' w
      Negative w -> "not " ++ prWire' w

    polarized i c = \case
      Positive (Circuit.wire -> 0) ->
        ( "True", "")
      Negative (Circuit.wire -> 0) ->
        ( "False", "")
      Positive w ->
        ( prWire' w, "")
      Negative w ->
        ( c : show i
        , indent 8 (c : show i) ++
          " = not " ++ prWire' w ++ "\n"
        )

    prOutputs =
      let
        (os, xs) =
          unzip $ map (\o -> polarized o 'o' $ outputWire o) outputs
      in
        concat xs ++ "\n    returnA -<\n" ++
        prMultiLineTuple 6 os

-----------------------------------------------------------------------------

prTuple
  :: [String] -> String

prTuple = \case
  []   -> "()"
  [x]  -> x
  x:xr -> "(" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ")"

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
  "      " ++ prWire cfm (termOutputWire t) ++ " <- " ++
  (case reverse $ termInputWires t of
     []     ->
       ("arr (const " ++) $ (++ ") -< ()\n") $
       if
         | termName t == "true"  -> "True"
         | termName t == "false" -> "False"
         | isPredicate t        -> "p_" ++ termName t
         | otherwise            -> "f_" ++ termName t
     (x:xr) ->
       "arr " ++
       (iterate (("(uncurry " ++) . (++ ")"))
         ((if isPredicate t then "p_" else "f_") ++ termName t)
           !! length xr) ++ " -< " ++
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
      | loopedInput i -> "c_" ++ inputName i
      | otherwise     -> "s_" ++ inputName i
    Right t
      | isPredicate t -> 'b' : show w
      | otherwise     -> 'w' : show w

-----------------------------------------------------------------------------
