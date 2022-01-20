----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import Config (Configuration(..), parseArguments)

import EncodingUtils (initEncoding)

import FileUtils (loadCFM, writeContent)

import TSL (implement)
import TSL (decodeOutputAP) 
import TSL (decodeInputAP)
import TSL (tslFormula)
import qualified TSL as T (Formula(..))
import Data.Either 
import Data.Char 
import Data.List 
import Data.Tuple

import Hanoi 
  ( HOA(..)
  , parse
  )
import System.Environment
import Data.Maybe
import Data.Tuple
import Data.List as List (intercalate, sortOn)

import Finite (Finite, FiniteBounds, index, offset, v2t, values)

import qualified Data.Map as M
import Data.List as List (intercalate, sortOn)
import Data.Maybe (maybeToList)
import Data.Set as Set (Set, elems, toList)
import Finite (Finite, FiniteBounds, index, offset, v2t, values)
import Hanoi
  ( AcceptanceSet
  , AcceptanceType(..)
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
  , State
  )
import Hanoi (Formula(..))

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, output, codeTarget, moduleName, functionName} <- parseArguments
  c <- readFile $ fromJust input
  let hoa = parse c
  putStrLn $ either id printHOA hoa
  --cfm <- loadCFM input

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' to as a 'String'

printHOA :: HOA -> String
printHOA = unlines . printHOALines

-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' as a list of 'String's representing
-- different lines

stripQuotes :: String -> String
stripQuotes =
  init. tail

printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let 
    apNamesSorted = map (\(l, idx) -> (atomicPropositionName l, idx)) $ sortOn (atomicPropositionName. fst) $ zip values [0..]
    apNamesMap = M.fromList $ map swap apNamesSorted

    printState :: FiniteBounds HOA => M.Map Int String -> State -> [String]
    printState nameMap s =
      unwords (
        ["\nif (currentState == "]
        ++
        maybeToList (printLabel <$> stateLabel s)
        ++
        [strInd s]
        ++ [") :"]
        ++
        (case stateAcceptance s of
          Just aSets -> [brCurly $ unwords (map strInd $ elems aSets)]
          Nothing    -> []
        )
      )
      :
      map (("  " ++) . printEdge) (toList $ edges s)

    printEdge ::
          FiniteBounds HOA
      => ([State], Maybe Label, Maybe (Set AcceptanceSet))
      -> String
    printEdge edge =
      let (target, label, aSets) = edge
      in
      unwords . concat $
        [ maybeToList $ printLabel <$> label
        , ["\n  currentState = "] ++ [printStateConj target]
        , maybeToList $ printAcceptanceSets <$> aSets
        ]

    printAcceptanceSets :: FiniteBounds HOA => Set AcceptanceSet -> String
    printAcceptanceSets aSets =
      brCurly $ unwords $ map strInd $ toList aSets


    printLabel :: FiniteBounds HOA => Label -> String
    printLabel label = "if (" ++ printFormula strInd2 label

    printStateConj :: FiniteBounds HOA => [State] -> String
    printStateConj = intercalate " & " . map strInd

    strInd2 = strIndWithMap apNamesMap
  in
    concatMap (printState apNamesMap) values

-----------------------------------------------------------------------------
-- | Different library related printing methods

strIndWithMap :: (Finite HOA a, FiniteBounds HOA) => M.Map Int String -> a -> String
strIndWithMap nameMap a = translateToTSL $ nameMap M.! (index a - offset (v2t a))

strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = (show (index a - offset (v2t a)))



wrap :: String -> String -> String -> String
wrap prefix suffix s = prefix ++ s ++ suffix


brRound :: String -> String
brRound = wrap "(" ")"

brBox :: String -> String
brBox = wrap "[" "]"

brCurly :: String -> String
brCurly = wrap "{" "}"

translateToTSL :: String -> String
translateToTSL t = 
  if isPrefixOf "p0" t
  then generateTSLString T.Check decodeInputAP t
  else generateTSLString (uncurry T.Update) decodeOutputAP t

generateTSLString :: forall a b. Show a => _ -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either (show) (\t -> (tslFormula id $ tslType t)) $
    (decoder) x
-----------------------------------------------------------------------------
-- | 'printFormula' prints a 'Formula' as a 'String'. Note that the way
-- of printing these formulas is HOA specific and therefore not part of
-- the HOA.Formula module.

printFormula :: (a -> String) -> Formula a -> String
printFormula showVar = \case
  FTrue -> "t"
  FFalse -> "f"
  FVar a -> showVar a
  FNot f -> "!" ++ printSubFormula f
  FAnd fs ->
    intercalate " & " $ fmap printSubFormula fs
  FOr fs ->
    intercalate " | " $ fmap printSubFormula fs

  where
    printSubFormula = (brRound . printFormula showVar)

