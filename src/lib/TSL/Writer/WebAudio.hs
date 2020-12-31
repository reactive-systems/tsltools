
{-# LANGUAGE

    RecordWildCards
  , ImplicitParams
  , LambdaCase
  , FlexibleContexts
  , ViewPatterns
  , MultiWayIf

  #-}

-----------------------------------------------------------------------------

module TSL.Writer.WebAudio
  ( implement
  ) where

-----------------------------------------------------------------------------

import Control.Exception
  ( assert
  )

import Data.List
  ( intersperse
  )

import Data.Char
  ( toLower
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
  , Input
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
    [ let
        createArgList = map ("s_" ++)
      in
        "function control" ++ 
        entryParamDestructor 0 (createArgList (map (inputName) inputs)) ++ 
        "{"
    , ""
    , indent 4 "// Terms"
    , concatMap (prTerm' cfm) terms
    , indent 4 ("let innerCircuit = controlCircuit" ++ prTuple (map (prWire cfm . controlInputWire) is) ++ ";")
    , indent 4 "// Output Cells"
    , concatMap prOutputCell outputs
    , indent 4 "// Switches"
    , concatMap prSwitch outputs
    -- RETURN STATEMENT
    , indent 4 $ "return" ++
      prReturn (map (("o_" ++) . outputName) outputs)
    , "}"
    , ""
    , replicate 77 '/'
    , ""
    , concatMap (prSwitchImpl cfm) outputs
    , prCircuitImpl control
    , replicate 77 '/'
    , "//IMPLEMENTED FUNCTIONS"
    , implementWebAudio inputVars outputVars
    ]

  where
    inputVars = map inputName inputs
    outputVars = map outputName outputs

    prOutputCell o =
      indent 4 $ "let c_" ++ outputName o ++
      " = s_" ++ outputName o ++ ";\n"

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
      indent 4 ("let o_" ++ outputName o ++ " = ") ++
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

data JSElem = NullElem
            | JSBool {varName :: String}
            | Waveform {varName :: String}
            | Change {varName :: String}
            | TurnOn {varName :: String}
            | TurnOff {varName :: String}
            | Cell {varName :: String}
            | Note {varName :: String}
            deriving (Eq)

-- TODO
actionName :: JSElem -> String
actionName (NullElem) = ""
actionName (JSBool _) = "" -- Should be error
actionName (Cell _) = "" -- Should be error
actionName (Waveform _) = "change"
actionName (Change _) = "change"
actionName (Note _) = "click"
actionName _ = undefined

strToJSElem :: String -> JSElem
strToJSElem = \case
-- TODO: Allow eventlistener on waveform change.
-- Implement rest of datatype.
  "False"       -> JSBool "false"
  "True"        -> JSBool "true"
  "Sawtooth"    -> Waveform "Sawtooth"
  "Square"      -> Waveform "Square"
  "Sine"        -> Waveform "Sine"
  "Triangle"    -> Waveform "Triangle"
  "AMFreq"      -> Change "amFreq"
  "Gain"        -> Change "Gain"
  "AMSynthesis" -> Cell "amSynthesis"
  "FMSynthesis" -> Cell "fmSynthesis"
  "LFO"         -> Cell "LFO"
  "Waveform"    -> Cell "waveform"
  note          -> Note note

implementWebAudio :: [String] -> [String] -> String 
implementWebAudio inputs outputs = 
  unlines $ functionImpl:(intersperse "\n" $ 
    map defineNotes (filter isNote eventableInputs) ++ 
    (map signalUpdateCode $ NullElem:eventableInputs))
    where 
      functionImpl :: String
      functionImpl = unlines
        ["function p_Change(input){return input;}"
        ,"function p_Press(input){return input;}"]

      inputSignals :: [JSElem]
      inputSignals = map strToJSElem inputs

      eventableInputs :: [JSElem]
      eventableInputs = filter eventable inputSignals

      outputStr :: String
      outputStr = show outputs

      -- TODO: implement rest
      eventable :: JSElem -> Bool
      eventable (Change _)  = True
      eventable (Note _)    = True
      eventable (TurnOn _)  = True
      eventable (TurnOff _) = True
      eventable _           = False

      isNote :: JSElem -> Bool
      isNote (Note _) = True
      isNote _        = False

      defineNotes :: JSElem -> String
      defineNotes (Note note) = 
        "const " ++ note ++ 
        " = document.getElementByID(" ++
        show note ++
        ");\n"
      defineNotes _ = ""

      signalUpdateCode :: JSElem -> String
      signalUpdateCode NullElem = saveOutput 0 NullElem
      signalUpdateCode var = 
        varName var ++
        ".addEventListener(\"" ++
        actionName var ++ "\", " ++
        "_ => {\n" ++
        saveOutput 4 var ++
        "\n});"

      saveOutput :: Int -> JSElem -> String
      saveOutput numIndents var = 
                      indent numIndents (outputStr ++   
                      " = control({\n") ++ 
                      dropLastTwo 
                        (concatMap 
                        (indent (numIndents + 4) . pipeSignal) 
                        inputSignals) ++
                      "});"
        where
          pipeSignal :: JSElem -> String
          pipeSignal (NullElem) = ""
          pipeSignal x = case eventable x of
            False -> showSignal ++ varShown ++ nl
            True -> case x == var of
              False -> showSignal ++ "false" ++ nl
              True  -> showSignal ++ "true" ++ nl
            where showSignal = "s_" ++ varName x ++ " : "
                  nl = ",\n"
                  
                  varShown :: String
                  varShown = case x of 
                    Waveform y -> "\"" ++ 
                      map toLower (varName x) ++ "\""
                    otherwise  -> varName x
          
          dropLastTwo :: String -> String
          dropLastTwo str = take (length str - 2) str
            

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
    , concatMap prLatchJS latches
    , "// Gates"
    , concatMap prGate gates
    , "// Outputs"
    , prOutputs
    ,"\n }"
    ]

  where
    isNeg = \case
      Positive _                  -> False
      Negative (Circuit.wire -> 0) -> False
      Negative _                  -> True

    -- No idea if this is correct
    prWire' x
      | Circuit.wire x <= length inputs = 
        let minusedOne = Circuit.wire x - 1
        in
          case minusedOne >= 0 of
            True  -> "cin" ++ show minusedOne
            False -> "cin0"
            -- False -> "cinNeg" ++ (show $ abs $ minusedOne)
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
        initVar wire = "var " ++ prWire' wire ++ ";\n"
      in initVar unwrapped ++ initVar ow

    globalGateVar :: Gate -> String
    globalGateVar g =
      let
        ow = gateOutput g :: Circuit.Wire
      in
        indent 8 $ "var " ++ (prWire' ow) ++ ";\n"

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
  [x]  -> "(" ++ x ++ ")"
  x:xr -> "(" ++ x ++ concatMap ((',':) . (' ':)) xr ++ ")"

-----------------------------------------------------------------------------
prReturn
  :: [String] -> String

prReturn = \case
  []   -> "[]"
  [x]  -> "[x]"
  x:xr -> "[" ++ x ++ concatMap ((',':) . (' ':)) xr ++ "];"

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

entryParamDestructor
  :: Int -> [String] -> String

entryParamDestructor n = \case
  []   -> indent n "()"
  [x]  -> indent n "({" ++ x ++ "})"
  x:xr ->
    indent n ("({ " ++ x ++ "\n") ++
    concatMap (indent n . (", " ++) . (++ "\n")) xr ++
    indent n "})"
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