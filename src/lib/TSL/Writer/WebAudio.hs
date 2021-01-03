
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
  ( intersperse
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
    , ""
    , indent 4 "// Output Cells"
    , concatMap prOutputCell outputs
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
    , "// IMPLEMENTED FUNCTIONS"
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

data JSElem = NullElem
            | Change {varName :: String}
            | Cell {varName :: String}
            | Note {varName :: String}
            deriving (Eq)

actionName :: JSElem -> String
actionName (NullElem) = ""
actionName (Cell _) = "" -- Should be error
actionName (Change _) = "change"
actionName (Note _) = "click"

strToJSElem :: String -> JSElem
strToJSElem = \case
-- TODO: Allow eventlistener on waveform change.
  "amFreq"      -> Change "amFreq"
  "gain"        -> Change "gain"
  "amSynthesis" -> Cell "amSynthesis"
  "fmSynthesis" -> Cell "fmSynthesis"
  "lfo"         -> Cell "lfo"
  "waveform"    -> Cell "waveform"
  note          -> Note note

implementWebAudio :: [String] -> [String] -> String 
implementWebAudio inputs outputs = 
  unlines $ intersperse "\n" $ concat
    [ functionImpl
    , map defineNotes $ filter isNote eventableInputs
    , map signalUpdateCode $ NullElem:eventableInputs
    , map buttonEventListeners outputSignals
    ]
    where 
      functionImpl :: [String]
      functionImpl = 
        ["function p_change(input){return input;}"
        ,"function p_press(input){return input;}"
        ,"function f_True(){return true;}"
        ,"function f_False(){return false;}"
        ,"function f_sawtooth(){return \"sawtooth\";}"
        ,"function f_sine(){return \"sine\";}"
        ,"function f_square(){return \"square\";}"
        ,"function f_triangle(){return \"triangle\";}"
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

      eventable :: JSElem -> Bool
      eventable (Change _)  = True
      eventable (Note _)    = True
      eventable _           = False

      isNote :: JSElem -> Bool
      isNote (Note _) = True
      isNote _        = False

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
                      "});"
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


      buttonEventListeners :: JSElem -> String
      buttonEventListeners = \case
        Cell "amSynthesis" -> 
          unlines [ "amOnBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(amSynthesis) {return};"
                  , saveOutput 4 NullElem
                  , "});\n"
                  , "amOffBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(!amSynthesis) {return};"
                  , saveOutput 4 NullElem
                  , "});"
                  ]
        Cell "fmSynthesis" -> 
          unlines [ "fmOnBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(fmSynthesis) {return};"
                  , saveOutput 4 NullElem
                  , "});\n"
                  , "fmOffBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(!fmSynthesis) {return};"
                  , saveOutput 4 NullElem
                  , "});"
                  ]
        Cell "lfo" -> 
          unlines [ "lfoOnBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(lfo) {return};"
                  , saveOutput 4 NullElem
                  , "});\n"
                  , "lfoOffBtn.addEventListener(\"click\", _ => {"
                  , indent 4 "if(!lfo) {return};"
                  , saveOutput 4 NullElem
                  , "});"
                  ]
        _ -> ""
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