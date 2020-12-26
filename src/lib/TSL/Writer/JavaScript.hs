{-# LANGUAGE

    RecordWildCards
  , ImplicitParams
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns
  , MultiWayIf

  #-}

-----------------------------------------------------------------------------

module TSL.Writer.JavaScript
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
  , Latch
  , Gate
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
    , "// JavaScript Interface for " ++ fName ++ "."
    , "//"
    , ""
    , replicate 77 '/'
    , ""
    , "function "++fName ++ 
      prMultiLineTuple 1 (map inputName
      $ filter(not.loopedInput) inputs)++ "{"
    , ""
    , concatMap (prTerm' cfm) terms
    , indent 4 ("let innerCircuit = controlCircuit" ++ prTuple (map (prWire cfm . controlInputWire) is) ++ ";")
    , indent 4 "// Output Cells"
    , concatMap prOutputCell outputs
    , indent 4 "// Switches"
    , concatMap prSwitch outputs
      -- COMMENTED: return statement for main fxn
      -- ++ "    return ntuple" ++
      -- prtuple (map (("o_" ++) . outputname) outputs)
    , "}"
    , ""
    , replicate 77 '/'
    , ""
    , concatMap (prSwitchImpl cfm) outputs
    ]
    ++
    prCircuitImpl control

  where
    prOutputCell o =
      "      c_" ++ outputName o++
      " = o_" ++ outputName o ++ ";\n"

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
      indent 6 ("o_" ++ outputName o ++ " = ") ++
      (outputName o) ++ "Switch" ++
      prTuple (map (\(w,x) -> "[" ++ prWire cfm w ++ ", innerCircuit[" ++ show x ++ "]]")
           $ outputSwitch o) ++ ";\n\n"

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
      [ "function "++ outputName o ++ "Switch"
      , prMultiLineTuple 7 (map (\i -> "p" ++ show i)[0,1..n-1])++ "{"
      , concatMap(\j -> 
            indent 4 "const r"++show j++
            " = p"++show j++"[1] ?"++
            " p"++show j++"[0]"++
            " : " ++
            (if n == j + 2 then "p" else "r")++
            show (j+1)++ 
            (if n == j + 2 then "[0]; \n" else ";\n ")
            )
            [n-2,n-3..0] ++ indent 4 "return r0;"
      ,"}"
      ,""]

-- -----------------------------------------------------------------------------

prCircuitImpl
  :: Circuit -> String

prCircuitImpl Circuit{..} =
  unlines
    [ concatMap globalLatchVar latches
    , concatMap globalGateVar gates
    , concatMap latchVarInit latches
    , "function controlCircuit"
    , prMultiLineTuple 4
        (map (("cin" ++) . show) inputs) ++ 
    "{"
    , "/*"
    , concatMap prLatch latches ++ "*/"
    , concatMap prLatchJS latches
    , "// Gates"
    , concatMap prGate gates
    , "// Outputs"
    , prOutputs
    ,"\n }"
    ,"/*"
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
    , 
    "*/"
    ]

  where
    isNeg = \case
      Positive _                  -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                  -> True

    prWire' x
      | Circuit.wire x <= length inputs = "cin" ++ show (Circuit.wire x - 1)
      | otherwise                      = 'w' : show x
    
    latchVarInit :: Latch -> String
    latchVarInit l =
      let
        iw = latchInput l  :: Invertible Circuit.Wire
        -- HACK
        -- Also need to check this is true.
        unwrapped = case iw of
          Negative w -> w
          Positive w -> w
      in
        prWire' unwrapped ++ " = false;\n"
    
    globalLatchVar :: Latch -> String
    globalLatchVar l = 
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire
        -- HACK
        unwrapped = case iw of
          Negative w -> w
          Positive w -> w
        
        initVar :: Circuit.Wire -> String
        initVar wire = "let " ++ prWire' wire ++ ";\n"
      in initVar unwrapped ++ initVar ow

    globalGateVar :: Gate -> String
    globalGateVar g =
      let
        ow = gateOutput g :: Circuit.Wire
      in
        indent 8 $ "let " ++ (prWire' ow) ++ ";\n"

    prLatchJS :: Latch -> String
    prLatchJS l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        polarization = case iw of
          Negative w -> "!" ++ prWire' w
          Positive w -> prWire' w
      in
        indent 6 (prWire' ow) ++
        " = " ++ polarization ++ ";\n"

    prLatch :: Latch -> String
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
        poled iwA ++ " && " ++ poled iwB ++ ";\n"

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
        
        addLet :: String -> String
        addLet base = case base of
          ""    -> ""
          base' -> "const " ++ base'' ++ ";\n"
            where base'' = takeWhile (/='\n') base'
          
      in
        concat (map addLet xs)
        ++ "\n    return " ++
        prList os ++ ";"

-----------------------------------------------------------------------------

prTuple
  :: [String] -> String

prTuple = \case
  []   -> "()"
  [x]  -> x
  x:xr -> "(" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ")"

-----------------------------------------------------------------------------

prList
  :: [String] -> String

prList = \case
  []   -> "[]"
  [x]  -> "[x]"
  x:xr -> "[" ++ x ++ concatMap ((',':) . (' ':)) xr ++ "]"

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
  [x]  -> indent n "(" ++ x ++ ")"
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
  "      let " ++ prWire cfm (termOutputWire t) ++ " = " ++
  (case reverse $ termInputWires t of
     []     ->
     --need to fix for w10 = -1, w11 = 0
      (++ "();\n") $
       if
         | termName t == "true"  -> "true"
         | termName t == "false" -> "false"
         | isPredicate t        -> "p_" ++ termName t
         | otherwise            -> "f_" ++ termName t
     (x:xr) ->
       (iterate (("" ++) . (++""))
         ((if isPredicate t then "p_" else "f_") ++ termName t)
           !! length xr) ++ "(" ++
       prT (prWire cfm x) xr ++ ");\n")

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
