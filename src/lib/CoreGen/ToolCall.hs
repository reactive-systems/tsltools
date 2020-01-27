-----------------------------------------------------------------------------
-- |
-- Module      :  CoreGen.ToolCall
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Uses generated core canidates and call a synthesizer to generate 
-- a core
--
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module CoreGen.ToolCall
  ( generateCore
  , Core(..)
  , generateCoreFromFile
  ) where

-----------------------------------------------------------------------------
import CoreGen.CoreGen (Query, getCores)
import TSL.Reader (fromTSLtoTSLSpec)
import TSL.Specification (TSLSpecification)
import TSL.ToString (tslSpecToString)

import External.Context (Context, tslSpecRealizable, tslSpecSatisfiable)
import External.ToolCalls (strixContext)

-----------------------------------------------------------------------------
--
--Notion of different core types
-- 
data Core
  = NaC
  | Unsat TSLSpecification
  | Unrez TSLSpecification

-----------------------------------------------------------------------------
--
-- Generates (eventually) a core given a TSL Specification using a simple
-- realizabilty test
--
generateCore :: Context -> TSLSpecification -> IO Core
generateCore context tsl = do
  let queries = getCores tsl
  genCore' queries
  where
    genCore' :: [Query] -> IO Core
    genCore' [] = return NaC
    genCore' (q:qr) = do
      sat <- tslSpecSatisfiable context q
      if not sat
        then return (Unsat q)
        else do
          rel <- tslSpecRealizable context q
          if not rel
            then return (Unrez q)
            else do
              genCore' qr

-----------------------------------------------------------------------------
--
-- Wraps the generate core Method into File IO
--
generateCoreFromFile :: FilePath -> IO ()
generateCoreFromFile path = do
  tsl <- readFile path
  case fromTSLtoTSLSpec tsl of
    Left err -> putStrLn (show err)
    Right spec -> do
      core <- generateCore strixContext spec
      case core of
        NaC -> print "Not unrealizable"
        Unrez s -> putStrLn ("UNREALIZABLE\n\n" ++ tslSpecToString s)
        Unsat s -> putStrLn ("UNSATISFIABLE\n\n" ++ tslSpecToString s)
