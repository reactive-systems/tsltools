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
            | Cell {varName :: String}
            | Button {varName :: String}
            | Note {varName :: String}
            deriving (Eq)

actionName :: JSElem -> String
actionName (Note _) = "click"
actionName _ = ""

strToJSElem :: String -> JSElem
strToJSElem = \case
  -- Cells
  "amFreq"      -> Cell "amFreq"
  "fmFreq"      -> Cell "fmFreq"
  "amSynthesis" -> Cell "amSynthesis"
  "fmSynthesis" -> Cell "fmSynthesis"
  "lfo"         -> Cell "lfo"
  "lfoFreq"     -> Cell "lfoFreq"
  "lfoDepth"    -> Cell "lfoDepth"
  "waveform"    -> Cell "waveform"
  -- Buttons
  "amOnBtn"     -> Button "amOnBtn"
  "amOffBtn"    -> Button "amOffBtn"
  "fmOnBtn"     -> Button "fmOnBtn"
  "fmOffBtn"     -> Button "fmOffBtn"
  "lfoOnBtn"    -> Button "lfoOnBtn"
  "lfoOffBtn"   -> Button "lfoOffBtn"
  -- Notes
  note          -> Note note

implementWebAudio :: [String] -> [String] -> String 
implementWebAudio inputs outputs = 
  unlines [ "// Implemented Functions"
          , functionImpl
          , ""
          , "// Event listeners"
          , concatMap defineNotes reactiveNotes
          , concat $ intersperse "\n\n" $
            map signalUpdateCode (NullElem:eventableInputs)
          , ""
          , makeMidiTriggers reactiveNotes
          , ""
          , postlude
          ]
    where 
      -- TODO: add constant additions
      functionImpl :: String
      functionImpl = unlines
        ["function p_play(input){return input;}"
        ,"function p_press(input){return input;}"
        ,"function f_True(){return true;}"
        ,"function f_False(){return false;}"
        ,"function f_sawtooth(){return \"sawtooth\";}"
        ,"function f_sine(){return \"sine\";}"
        ,"function f_square(){return \"square\";}"
        ,"function f_triangle(){return \"triangle\";}"
        ,"function f_inc10(arg){return arg+10;}"
        ,"function f_dec10(arg){return Math.max(arg-10,0);}"
        ]

      inputSignals :: [JSElem]
      inputSignals = map strToJSElem inputs

      outputSignals :: [JSElem]
      outputSignals = map strToJSElem outputs

      eventableInputs :: [JSElem]
      eventableInputs = filter eventable inputSignals

      outputStr :: String
      outputStr = prList $ 
                  map (varName . strToJSElem) outputs

      updateVarsToUI :: String
      updateVarsToUI = "updateVarsToUI();"

      eventable :: JSElem -> Bool
      eventable (Button _)  = True
      eventable (Note _)    = True
      eventable _           = False

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
        indent 4 "const noteSignal = 's_' + note;\n" ++
        indent 4 inputTemplate ++ 
        indent 4 "inputTemplate[noteSignal] = true;\n" ++
        indent 4 "if(triggerNotes.has(noteSignal)){\n" ++ 
        indent 8 (outputStr ++ " = control(inputTemplate);\n") ++
        indent 4 "}\n" ++ 
        "}"
        where 
          triggerNoteSetInit = "const triggerNotes = new Set(" ++
                               prList (map varName notes) ++ 
                               ");\n"
          fxnHeader = "function reactiveUpdateOnMIDI" ++
                      "(note, velocity){\n"
          inputTemplate = 
            "const inputTemplate = " ++
            prDictFormatted 8 (map inSigInit notes) ++
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
    , concatMap prLatchJS latches
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
            False -> "cin0"
            -- False -> "cinNeg" ++ (show $ abs $ minusedOne)
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
        initVar wire = "var " ++ prWire' wire ++ ";\n"
      in initVar unwrapped ++ initVar ow

    globalGateVar :: Gate -> String
    globalGateVar g =
      let
        ow = gateOutput g :: Circuit.Wire
      in
        "var " ++ (prWire' ow) ++ ";\n"

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