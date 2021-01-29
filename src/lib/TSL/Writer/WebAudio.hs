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

import Data.List
  (
    intersperse
  )

import TSL.CFM
  ( Output
  , Wire
  , Term
  , CFM(..)
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

-----------------------------------------------------------------------------

type ModuleName = String
type FunctionName = String

-----------------------------------------------------------------------------

implement
  :: ModuleName
  -> FunctionName
  -> CFM
  -> String

implement _ _ cfm@CFM{..} =
  let ?bounds = cfm in
  unlines
    [ "// This module is intended to be used with the TSL Synthesis Synthesizer."
    , "// Visit the project repository: https://github.com/Barnard-PL-Labs/TSLSynthesizer"
    , ""
    , let
        createArgList = map ("s_" ++)
      in
        "function control" ++ 
        entryParamDestructor 0 (createArgList (map (inputName) inputs)) ++ 
        "{"
    , ""
    , indent 4 "// Cells"
    , concatMap prOutputCell outputs
    , indent 4 "// Terms"
    , concatMap (prTerm' cfm) terms
    , indent 4 ("let innerCircuit = controlCircuit" ++ prTuple (map (prWire cfm . controlInputWire) is) ++ ";")
    , ""
    , indent 4 "// Switches"
    , concatMap prSwitch outputs
    -- RETURN STATEMENT
    , prReturn (map (("o_" ++) . outputName) outputs)
    , "}"
    , ""
    , replicate 77 '/'
    , ""
    , concatMap (prSwitchImpl cfm) outputs
    , prCircuitImpl control
    , replicate 77 '/'
    , ""
    , implementWebAudio inputVars outputVars
    ]

  where
    inputVars = map inputName inputs
    outputVars = map outputName outputs

    prOutputCell o =
      indent 4 $ "let c_" ++ outputName o ++
      " = s_" ++ outputName o ++ ";\n"

    is = Circuit.inputs control

    prSwitch o =
      indent 4 ("let o_" ++ outputName o ++ " = ") ++
      (outputName o) ++ "Switch" ++
      prTuple (map (\(w,x) -> "[" ++ prWire cfm w ++ ", innerCircuit[" ++ show x ++ "]]")
           $ outputSwitch o) ++ ";\n\n"

-----------------------------------------------------------------------------

-- TODO: make datatype more structured & intuitive.
data JSElem = NullElem
            | Keyboard {varName :: String}
            | Cell {varName :: String}
            | Select {varName :: String}
            | Signal {varName :: String}
            | Button {varName :: String}
            | Note {varName :: String}
            deriving (Show, Eq)

actionName :: JSElem -> String
actionName (Keyboard _) = "click"
actionName (Select _)   = "change"
actionName (Button _)   = "click"
actionName (Note _)     = "click"
actionName _ = ""

strToJSElem :: String -> JSElem
strToJSElem = \case
  -- Cells
  "amFreq"             -> Cell "amFreq"
  "fmFreq"             -> Cell "fmFreq"
  "amSynthesis"        -> Cell "amSynthesis"
  "fmSynthesis"        -> Cell "fmSynthesis"
  "filterOn"           -> Cell "filterOn"
  "harmonizerOn"       -> Cell "harmonizerOn"
  "arpeggiatorOn"      -> Cell "arpeggiatorOn"
  "lfo"                -> Cell "lfo"
  "lfoFreq"            -> Cell "lfoFreq"
  "lfoDepth"           -> Cell "lfoDepth"
  "waveform"           -> Cell "waveform"
  "harmonizerInterval" -> Cell "harmonizerInterval"
  "filterType"         -> Cell "filterType"
  "filterCutoff"       -> Cell "filterCutoff"
  "filterQ"            -> Cell "filterQ"
  "arpeggiatorStyle"   -> Cell "arpeggiatorStyle"
  "arpeggiatorRate"    -> Cell "arpeggiatorRate"
  -- Buttons
  "amOnBtn"            -> Button "amOnBtn"
  "amOffBtn"           -> Button "amOffBtn"
  "fmOnBtn"            -> Button "fmOnBtn"
  "fmOffBtn"           -> Button "fmOffBtn"
  "lfoOnBtn"           -> Button "lfoOnBtn"
  "lfoOffBtn"          -> Button "lfoOffBtn"
  "filterOnBtn"        -> Button "filterOnBtn"
  "filterOffBtn"       -> Button "filterOffBtn"
  "harmonizerOnBtn"    -> Button "harmonizerOnBtn"
  "harmonizerOffBtn"   -> Button "harmonizerOffBtn"
  "arpeggiatorOnBtn"   -> Button "arpeggiatorOnBtn"
  "arpeggiatorOffBtn"  -> Button "arpeggiatorOffBtn"
  -- Signals
  "noteVelocity"       -> Signal "noteVelocity"
  -- Select
  "waveformControl"    -> Select "waveformControl"
  -- Notes
  note                 -> Note note

implementWebAudio :: [String] -> [String] -> String 
implementWebAudio inputs outputs = 
  unlines [ "// Implemented Functions"
          , functionImpl
          , ""
          , "// Notes"
          , concatMap defineNotes reactiveNotes
          , "// Reactive Updates"
          , concat $ intersperse "\n\n" $
            map signalUpdateCode (NullElem:eventableInputs)
          , ""
          , makeMidiTriggers reactiveNotes
          , ""
          , postlude
          ]
    where 
      functionImpl :: String
      functionImpl = unlines
        ["function p_play(input){return input;}"
        ,"function p_press(input){return input;}"
        ,"function p_change(input){return input;}"
        ,"function p_veloBelow50(velocity){return velocity<=50}"
        ,"function p_veloAbove50(velocity){return velocity>50}"
        ,"function f_True(){return true;}"
        ,"function f_False(){return false;}"
        ,"function f_sawtooth(){return \"sawtooth\";}"
        ,"function f_sine(){return \"sine\";}"
        ,"function f_square(){return \"square\";}"
        ,"function f_triangle(){return \"triangle\";}"
        ,"function f_upStyle(){return \"up\";}"
        ,"function f_downStyle(){return \"down\";}"
        ,"function f_upDownStyle(){return \"up-down\";}"
        ,"function f_randomStyle(){return \"random\";}"
        ,"function f_lowpass(){return \"lowpass\";}"
        ,"function f_highpass(){return \"highpass\";}"
        ,"function f_bandpass(){return \"bandpass\";}"
        ,"function f_toggle(input){return !input};"
        ,"function f_inc1000(arg){return arg+1000;}"
        ,"function f_dec1000(arg){return Math.max(arg-1000,0);}"
        ,"function f_inc100(arg){return Math.min(arg+100,10000);}"
        ,"function f_dec100(arg){return Math.max(arg-100,20);}"
        ,"function f_inc10(arg){return arg+10;}"
        ,"function f_dec10(arg){return Math.max(arg-10,0);}"
        ,"function f_inc1(arg){return arg+1;}"
        ,"function f_dec1(arg){return Math.max(arg-1,0);}"
        ,"function f_inc1max12(arg){return Math.min(arg+1,12);}"
        ,"function f_dec1min12(arg){return Math.max(arg-1,-12);}"
        ,"function f_getWaveformVal(node){return waveformControl.value}"
        ,"function f_getArpType(node){return arpeggiatorStyleControl.value}"
        ]
  
      keyboardElem :: JSElem
      keyboardElem = Keyboard "keyboardNode"

      inputSignals :: [JSElem]
      inputSignals = keyboardElem:(map strToJSElem inputs)

      eventableInputs :: [JSElem]
      eventableInputs = filter eventable inputSignals

      outputStr :: String
      outputStr = prList $ 
                  map (varName . strToJSElem) outputs

      updateVarsToUI :: String
      updateVarsToUI = "updateVarsToUI();"

      eventable :: JSElem -> Bool
      eventable (Button _)    = True
      eventable (Select _)    = True
      eventable (Note _)      = True
      eventable (Keyboard _ ) = True
      eventable _             = False

      isNote :: JSElem -> Bool
      isNote (Note _) = True
      isNote _        = False

      reactiveNotes :: [JSElem]
      reactiveNotes = filter isNote eventableInputs

      defineNotes :: JSElem -> String
      defineNotes (Note note) = 
        "var " ++ note ++ 
        " = document.getElementById(" ++
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
                      "});\n" ++
                      indent numIndents updateVarsToUI

        where
          pipeSignal :: JSElem -> String
          pipeSignal (NullElem) = ""
          -- XXX
          pipeSignal (Signal _) = "s_noteVelocity : 50,\n"
          pipeSignal x = case eventable x of
            False -> signalShown ++ varName x ++ nl
            True -> case x == var of
              False -> signalShown ++ "false" ++ nl
              True  -> signalShown ++ "true" ++ nl
            where
              nl = ",\n"

              signalShown :: String
              signalShown = "s_" ++ varName x ++ " : "
      
          dropLastTwo :: String -> String
          dropLastTwo str = take (length str - 2) str

      makeMidiTriggers :: [JSElem] -> String
      makeMidiTriggers [] = ""
      makeMidiTriggers notes =
        triggerNoteSetInit ++ 
        fxnHeader ++
        indent 4 "const noteSignal = 's_note' + note;\n" ++
        indent 4 "const noteVelocity = velocity;\n" ++
        indent 4 inputTemplate ++ 
        indent 4 "inputTemplate[noteSignal] = true;\n" ++
        indent 4 "if(triggerNotes.has(noteSignal)){\n" ++ 
        indent 8 (outputStr ++ " = control(inputTemplate);\n") ++
        indent 8 "updateVarsToUI();\n" ++
        indent 4 "}\n" ++
        "}"
        where 
          triggerNoteSetInit = "var triggerNotes = new Set(" ++
                               prList (map (surroundQuotes . varName) notes) ++ 
                               ");\n"
            where surroundQuotes str = "\"s_" ++ str ++ "\""
          fxnHeader = "function reactiveUpdateOnMIDI" ++
                      "(note, velocity){\n"
          inputTemplate = 
            "const inputTemplate = " ++
            prDictFormatted 8 (map inSigInit inputSignals) ++
            ";\n"
            where 
              inSigInit :: JSElem -> String
              inSigInit NullElem = ""
              inSigInit x = case eventable x of
                False -> signalShown ++ varName x
                True  -> signalShown ++ "false"
                where signalShown = "s_" ++ varName x ++ " : "

      postlude :: String
      postlude = unlines
        ["amOffBtn.click();"
        ,"fmOffBtn.click();"
        ,"lfoOffBtn.click();"]

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
      , prMultiLineTuple 4 (map (\i -> "p" ++ show i)[0,1..n-1])++ "{"
      , concatMap(\j -> 
            indent 4 "const r"++show j++
            " = p"++show j++"[1] ?"++
            " p"++show j++"[0]"++
            " : " ++
            (if n == j + 2 then "p" else "r")++
            show (j+1)++ 
            (if n == j + 2 then "[0]; \n" else ";\n")
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
    , ""
    , indent 4 "// Latches"
    -- I have no idea why commenting this works
    -- , concatMap prLatchJS latches
    , indent 4 "// Gates"
    , concatMap prGate gates
    , indent 4 "// Outputs"
    , prOutputs
    ,"\n }"
    ]

  where
    -- TODO: verify minusOne implementation.
    prWire' x
      | Circuit.wire x <= length inputs = 
        let minusedOne = Circuit.wire x - 1
        in
          case minusedOne >= 0 of
            True  -> "cin" ++ show minusedOne
            -- False -> "cin0"
            False -> "cinNeg" ++ (show $ abs $ minusedOne)
      | otherwise                      = 'w' : show x
    
    latchVarInit :: Latch -> String
    latchVarInit l =
      let
        iw = latchInput l  :: Invertible Circuit.Wire
        -- TODO: verify that variables start out as false/true.
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
        initVar wire = "var " ++ prWire' wire ++ " = false;\n"
      in initVar unwrapped ++ initVar ow

    globalGateVar :: Gate -> String
    globalGateVar g =
      let
        ow = gateOutput g :: Circuit.Wire
      in
        "var " ++ (prWire' ow) ++ " = false;\n"

    prLatchJS :: Latch -> String
    prLatchJS l =
      let
        iw = latchInput l :: Invertible Circuit.Wire
        ow = latchOutput l :: Circuit.Wire

        polarization = case iw of
          Negative w -> "!" ++ prWire' w
          Positive w -> prWire' w
      in
        indent 4 (prWire' ow) ++
        " = " ++ polarization ++ ";\n"

    prGate g =
      let
        iwA = gateInputA g :: Invertible Circuit.Wire
        iwB = gateInputB g :: Invertible Circuit.Wire
        ow = gateOutput g :: Circuit.Wire
      in
        indent 4 (prWire' ow) ++ " = " ++
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
          base' -> indent 4 $ "const " ++ base'' ++ ";\n"
            where base'' = dropWhile (==' ') $
                           takeWhile (/='\n') base'
          
      in
        (concatMap addLet xs) ++ 
        "\n" ++
        prReturn os

-----------------------------------------------------------------------------
prList
  :: [String] -> String

prList = \case
  []   -> "[]"
  [x]  -> "[" ++ x ++ "]"
  x:xr -> "[" ++ x ++ concatMap ((',':) . (' ':)) xr ++ "]"

-----------------------------------------------------------------------------
prDictFormatted
  :: Int -> [String] -> String

prDictFormatted numIndents = \case
  []   -> "{}"
  [x]  -> "{" ++ x ++ "}"
  x:xr -> "{\n" ++ indent numIndents x ++ ",\n" ++
          concatMap joinNext xr ++ 
          indent (numIndents - 4) "}"
    where joinNext y = indent numIndents y ++ ",\n"

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
  []   -> indent 4 "return [];"
  [x]  -> indent 4 "return [" ++ x ++ "];"
  x:xs -> indent 4 "return [ " ++ 
          x ++ 
         concatMap addLine xs ++ 
         "];"
    where addLine = (('\n':indent 11 ", ") ++ )

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
indent
  :: Int -> String -> String

indent n x =
  iterate (' ':) x !! n

-----------------------------------------------------------------------------

prTerm'
  :: CFM -> Term -> String

prTerm' cfm@CFM{..} t =
  (indent 4 "let ") ++ prWire cfm (termOutputWire t) ++ " = " ++
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