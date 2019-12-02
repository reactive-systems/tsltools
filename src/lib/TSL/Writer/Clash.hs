-----------------------------------------------------------------------------
-- |
-- Module      :  TSL.Writer.Clash
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Clash code generation.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , ImplicitParams
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns
  , MultiWayIf

  #-}

-----------------------------------------------------------------------------

module TSL.Writer.Clash
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
    , "-- |"
    , "-- Module : " ++ mName
    , "--"
    , "-- Clash Interface for " ++ fName ++ "."
    , "--"
    , replicate 77 '-'
    , ""
    , "module " ++ mName
    , "  ( " ++ fName
    , "  ) where"
    , ""
    , replicate 77 '-'
    , ""
    , "import Clash.Prelude"
    , ""
    , replicate 77 '-'
    , ""
    , fName
    , "  :: HiddenClockReset domain gated synchronous"
    , rpf [] $
      concatMap prTermType (filter isPredicate (constants cfm)) ++
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
      ++
      concatMap
        ((++ "\n") . ("  s_" ++) . inputName)
        (filter (not . loopedInput) inputs)
    , "  ="
    , ""
    , "  let"
    , concatMap prOutputCell outputs
    , concatMap (prTerm' cfm) terms
    , "    cout ="
    , "      controlCircuit"
    , case is of
        []  -> "        $ pure $ pack ()\n"
        [x] -> "        " ++ prWire' (controlInputWire x) ++ "\n"
        _   ->
          concatMap
            ( (++ "\n") .
              ("        $ liftA2 (++#) " ++) .
              prWire' .
              controlInputWire
            )
            (init $ reverse is) ++
          "          " ++
          prWire' (controlInputWire $ last $ reverse is) ++
          "\n"
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
    prWire' w = case wireSource w of
      Left _  -> "(pack <$> " ++ prWire cfm w ++ ")"
      Right _ -> prWire cfm  w

    rpf a = \case
      []         -> reverse a
      [x]        -> reverse (x:a)
      '-':'>':xr -> reverse a ++ "=>" ++ xr
      x : y : xr -> rpf (x:a) (y:xr)

    prOutputCell o =
      "    c_" ++ outputName o ++
      " = register i_" ++ outputName o ++
      " o_" ++ outputName o ++ "\n"

    prOutputType = \case
      []     ->
        "     -- no output\n" ++
        "  -> Signal domain ()"
      [x]    ->
        "     -- " ++ outputName x ++ " (output)\n" ++
        "  -> Signal domain " ++ prResultType x
      (x:xr) ->
        "     -- outputs\n" ++
        "  -> ( -- " ++ outputName x ++ "\n" ++
        "       Signal domain " ++ prResultType x ++ "\n" ++
        concatMap prO xr ++
        "     )"

    prO o =
      "     , -- " ++ outputName o ++ "\n" ++
      "       Signal domain " ++ prResultType o ++ "\n"

    prInputTypes i =
      "     -- " ++ inputName i ++ " (input)\n" ++
      "  -> Signal domain " ++ prT (wireType $ inputWire i) ++ "\n"

    prInitType o =
      "     -- initial value: " ++
      outputName o ++ "\n" ++
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
      "     -- " ++ termName t ++ "\n" ++
      "  -> " ++ prChain (termType cfm t) ++ "\n"

    is = Circuit.inputs control

    prSwitch o =
      "    o_" ++ outputName o ++ " =\n" ++
      "      " ++ outputName o ++ "Switch\n" ++
      (case outputSwitch o of
         []               ->
           "        $ pure $ pack () \n"
         [(w,x)]          ->
           "        (fmap (pack . (! " ++ show x ++ ")) cout)\n" ++
           "        " ++ prWire cfm w ++ "\n"
         [(w,x), (w',x')] ->
           "        ( liftA2 (++#) (fmap (pack . (! " ++ show x ++ ")) cout)\n" ++
           "        $ fmap (pack . (! " ++ show x' ++ ")) cout\n" ++
           "        )\n" ++
           "        ( liftA2 (:>) " ++ prWire cfm w ++ "\n" ++
           "        $ liftA2 (:>) " ++ prWire cfm w' ++ "\n" ++
           "        $ pure Nil\n" ++
           "        )\n"
         (w,x):xr ->
           "        ( liftA2 (++#) (fmap (pack . (! " ++ show x ++ ")) cout)\n" ++
           concatMap prArg (init xr) ++
           "        $ fmap (pack . (! " ++ show (snd $ last xr) ++ ")) cout\n" ++
           "        )\n" ++
           "        ( liftA2 (:>) " ++ prWire cfm w ++ "\n" ++
           concatMap prChoice xr ++
           "        $ pure Nil\n" ++
           "        )\n")

    prArg (_,x) =
      "        $ liftA2 (++#) (fmap (pack . (! " ++ show x ++ ")) cout)\n"

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
  unlines
    [ "controlCircuit"
    , " :: HiddenClockReset domain gated synchronous"
    , " => Signal domain (BitVector " ++ show (length inputs) ++ ")"
    , " -> Signal domain (BitVector " ++ show (length outputs) ++ ")"
    , ""
    , "controlCircuit cin = "
    , "  let"
    , concatMap prLatch latches
      ++ concatMap prGate gates
      ++ prOutputs
    , let
        hasLatches   = not $ null $ latches
        hasGates     = not $ null $ gates
        hasInverters =
            any isNeg (map outputWire outputs)
          || any isNeg (map latchInput latches)
          || any isNeg (map gateInputA gates)
          || any isNeg (map gateInputB gates)
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
      Positive _                  -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                  -> True

    prWire' x
      | Circuit.wire x <= length inputs = "(fmap (! " ++ show (Circuit.wire x - 1) ++ ") cin)"
      | otherwise                      = 'w' : show x

    polarized i c = \case
      Positive (Circuit.wire -> 0) ->
        ("(pure high)", "")
      Negative (Circuit.wire -> 0) ->
        ("(pure low)", "")
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
        (vs, xs) =
          unzip $ map (\o -> polarized o 'o' $ outputWire o) outputs
      in
        concat xs ++ "  in" ++ compose (reverse vs)

    compose = \case
      []   -> "\n  pure $ pack ()"
      [v]  -> "\n    (fmap pack " ++ v ++ ")"
      v:vr -> "\n    liftA2 (++#) (fmap pack " ++ v ++ ") $" ++ compose vr

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
       ("pure " ++) $ (++ "\n") $
       if
         | termName t == "true"  -> "True"
         | termName t == "false" -> "False"
         | isPredicate t        -> "p_" ++ termName t
         | otherwise            -> "f_" ++ termName t
     (x:xr) ->
       (if isPredicate t
        then if null xr
             then "pack . p_"
             else "pack <$> (p_"
        else "f_") ++
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
      | loopedInput i -> "c_" ++ inputName i
      | otherwise     -> "s_" ++ inputName i
    Right t
      | isPredicate t -> 'b' : show w
      | otherwise     -> 'w' : show w

-----------------------------------------------------------------------------
