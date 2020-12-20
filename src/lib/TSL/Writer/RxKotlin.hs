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

import qualified Data.List as List

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
    [ "// Module : " ++ mName
    , "//"
    , "// Rxlotlin Interface for " ++ fName ++ "."
    , "//"
    , ""
    , replicate 77 '/'
    , ""
    , "module " ++ mName
    , "  ( " ++ fName
    , "  ) where"
    , ""
    , replicate 77 '/'
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
    -- , "  :: (Arrow signal, ArrowLoop signal)"
    -- , "     // cell implementation"
    -- , "  => (forall poly. poly -> signal poly poly)"
    -- , concatMap prTermType (filter isPredicate (constants cfm)) ++
    --   concatMap prTermType (predicates cfm) ++
    --   concatMap prTermType (filter (not . isPredicate) (constants cfm)) ++
    --   concatMap prTermType (functions cfm) ++
    --   concatMap prInitType outputs ++
    --   "     // arrow"
    -- , "  -> signal"
    -- , prInputType (filter (not . loopedInput) inputs)
    -- , prOutputType outputs
    -- , ""
    -- , fName
    -- , "  cell"
    -- , concatMap
    --     ((++ "\n") . ("  p_" ++) . termName)
    --     (filter isPredicate (constants cfm))
    --   ++
    --   concatMap
    --     ((++ "\n") . ("  p_" ++) . termName)
    --     (predicates cfm)
    --   ++
    --   concatMap
    --     ((++ "\n") . ("  f_" ++) . termName)
    --     (filter (not . isPredicate) (constants cfm))
    --   ++
    --   concatMap
    --     ((++ "\n") . ("  f_" ++) . termName)
    --     (functions cfm)
    --   ++
    --   concatMap
    --     ((++ "\n") . ("  i_" ++) . outputName)
    --     outputs
    -- , "  ="
    -- , ""
    -- , ""
    , "data class ntuple"++prUniqueTypes outputs
    ++prTupleClass outputs++"\n"
      -- prTupleClassPara outputs
    , ""
    -- , prMultiLineTuple 4
    --     ( map (("s_" ++) . inputName)
    --     $ filter (not . loopedInput) inputs
    --     ) ++ " ->"
    -- ,""
    , "private fun "++fName 
    , prMultiLineTuple 7 (map (\i -> inputName i ++": "++prI i)
      $ filter(not.loopedInput) inputs)++
     ":ntuple"++prUniqueTypes outputs++"{"
    , ""
    -- , prTuple( Set.fromList map(\i -> prResultType i) outputs)
      -- ,prMultiLineTuple 7 (map (\i -> "p" ++ show i ++": Pair<T,Boolean>")[0,1..n-1])++
      -- ":T{"

      -- prTuple (map (inputName)) NEED TO FIX THIS BOI
    , concatMap (prTerm' cfm) terms
    --,"      " ++ prTuple (map (("cout" ++) . show) os) ++ " <-"
    , "    val cout = controlCircuit" ++ prTuple (map (prWire cfm . controlInputWire) is)
    , ""
    , concatMap prOutputCell outputs
    , concatMap prSwitch outputs ++
      "    return ntuple" ++
      prTuple (map (("o_" ++) . outputName) outputs)
    , "}"
    , ""
    , replicate 77 '+'
    , concatMap (prSwitchImpl cfm) outputs
    ]
    ++
    prCircuitImpl control
    ++
    replicate 77 '='

  where
    prOutputCell o =
      "      c_" ++ outputName o++
      " = o_" ++ outputName o ++ "\n"

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
        "       // " ++ inputName x ++ " (input) " ++
        "        " ++ prT (wireType $ inputWire x)
      x:xr ->
        "       ( // " ++ inputName x ++ " (input) " ++
        "         " ++ prT (wireType $ inputWire x) ++ " " ++
        concatMap prI xr ++
        "       )"

    -- prI i =
    --   "       , // " ++ inputName i ++ " (input)\n" ++
    --   "         " ++ prT (wireType $ inputWire i) ++ "\n"
    prI i =
      prT (wireType $ inputWire i)


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
            --idk why but these dont work
      -- Int     -> "Int"
      -- String  -> "String"
      t       -> prType t

    prTermType t =
      "     // " ++ termName t ++ "\n" ++
      "  -> " ++ prChain (termType cfm t) ++ "\n"

    is = Circuit.inputs control
    os = Circuit.outputs control

    prSwitch o =
      indent 6 ("val o_" ++ outputName o ++ " = ") ++
      (outputName o) ++ "Switch" ++
      prTuple (map (\(w,x) -> "Pair(" ++ prWire cfm w ++ ", cout[" ++ show x ++ "])")
           $ outputSwitch o) ++ "\n\n"

    prTupleClass o = 
      prTuple (map(\i -> "val "++outputName i++":"++prResultType i) o)

    prTupleClassPara o = 
        let
           n = length o
           first = map(\i -> "operator fun component" ++show i++"():")[0,1..n-1]
           second =  map(\j -> prResultType j++" = "++ outputName j)o 
         in
      prMultiLineTupleCurly 7 (zipWith (++) first second)

    prUniqueTypes o =
        let
          l = map(\i -> prResultType i) outputs
        in
      prTupleCarrot( List.nub l)



-----------------------------------------------------------------------------

prSwitchImpl
  :: CFM -> Output -> String
--it can only have Pairs as inputs, cant handle anything else, 
prSwitchImpl CFM{..} o =
  let
    xs = outputSwitch o
    n = length xs
  in
    unlines
      [ "private fun <T>"++ outputName o ++ "Switch"
      , prMultiLineTuple 7 (map (\i -> "p" ++ show i ++": Pair<T,Boolean>")[0,1..n-1])++
      ":T{"
      -- , prMultiLineTuple 4
      --     ( map
      --         (\i -> "(s" ++ show i ++ ", b" ++ show i ++ ")")
      --         [0,1..n-2]
      --       ++
      --       ["(s" ++ show (n-1) ++ ", _)"]
      --     ) ++ " ->"
      -- , "  do"
      --
      --
      , concatMap(\j -> 
            indent 4 "val r"++show j++
            " = if (p"++show j++".second)"++
            " p"++show j++"first"++
            " else " ++
            (if n == j + 2 then "p" else "r")++
            show (j+1)++
            (if n == j + 2 then ".first \n" else "\n ")
            )
            [n-2,n-3..0] ++ indent 4 "return r0"
      ,"}"
      ,""
      -- , concatMap
      --     (\i -> 
      --       indent 4 "r" ++ show i ++
      --       " <- arr ite -< (b" ++ show i ++
      --       ", s" ++ show i ++ ", " ++
      --       (if n == i + 2 then "s" else "r") ++
      --       show (i+1) ++ ")\n"
      --     )
      --     [n-2,n-3..0] ++ indent 4 "return r0"
      -- , ""
      -- , "  where"
      -- , "    ite (b, t, e) = if b then t else e"
      -- , ""
      -- , replicate 77 '-'
      ]

-- -----------------------------------------------------------------------------

prCircuitImpl
  :: Circuit -> String

prCircuitImpl Circuit{..} =
  unlines
    [ "private fun controlCircuit"
    , prMultiLineTuple 4
        -- (map (("cin" ++) . show) inputs)
        -- i might need to make the ":Boolean" into TermType stuff
        (map (\i -> "cin" ++ show i ++": Boolean")
           $ inputs)++
    ": Array<Boolean>{"
        --I've ignored the Latches
    , concatMap prLatch latches
    , concatMap prGate gates ++ prOutputs
    ,"\n }"
    --I think no need to care about Latchers and Inverters for the moment
    --
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
    
    --im havent touched prLatch
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
      Positive (Circuit.wire -> 0) -> "true"
      Negative (Circuit.wire -> 0) -> "false"
      Positive w -> prWire' w
      Negative w -> "!" ++ prWire' w

    polarized i c = \case
      Positive (Circuit.wire -> 0) ->
        ( "true", "")
      Negative (Circuit.wire -> 0) ->
        ( "false", "")
      Positive w ->
        ( prWire' w, "")
      Negative w ->
        ( c : show i
        , indent 8 (c : show i) ++
          " = !" ++ prWire' w ++ "\n"
        )

    prOutputs =
      let
        (os, xs) =
          unzip $ map (\o -> polarized o 'o' $ outputWire o) outputs
      in
        concat xs ++ "\n    return arrayOf" ++
        prTuple os

-----------------------------------------------------------------------------

prTuple
  :: [String] -> String

prTuple = \case
  []   -> "()"
  [x]  -> x
  x:xr -> "(" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ")"

-----------------------------------------------------------------------------
prTupleCarrot
  :: [String] -> String

prTupleCarrot = \case
  []   -> "()"
  [x]  -> x
  x:xr -> "<" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ">"

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

prMultiLineTupleCurly
  :: Int -> [String] -> String

prMultiLineTupleCurly n = \case
  []   -> indent n "()"
  [x]  -> indent n x
  x:xr ->
    indent n ("{ " ++ x ++ "\n") ++
    concatMap (indent n . (", " ++) . (++ "\n")) xr ++
    indent n "}"
-----------------------------------------------------------------------------

indent
  :: Int -> String -> String

indent n x =
  iterate (' ':) x !! n

-----------------------------------------------------------------------------

prTerm'
  :: CFM -> Term -> String

prTerm' cfm@CFM{..} t =
  "      val " ++ prWire cfm (termOutputWire t) ++ " = " ++
  (case reverse $ termInputWires t of
     []     ->
     --need to fix for w10 = -1, w11 = 0
      (++ "()\n") $
       if
         | termName t == "true"  -> "true"
         | termName t == "false" -> "false"
         | isPredicate t        -> "p_" ++ termName t
         | otherwise            -> "f_" ++ termName t
     (x:xr) ->
       (iterate (("" ++) . (++""))
         ((if isPredicate t then "p_" else "f_") ++ termName t)
           !! length xr) ++ "(" ++
       prT (prWire cfm x) xr ++ ")\n")

  where
    prT s = \case
      []   -> s
      x:xr -> prT (s ++ ", " ++ prWire cfm x) xr

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
