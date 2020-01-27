----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Allows to generate unsat/unrez cores 
--
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------
import CoreGen (Core(..), generateCore)

import External.ToolCalls (strixContext)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import TSL (fromTSLtoTSLSpec, tslSpecToString)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [tslPath] -> do
      tslEx <- doesFileExist tslPath
      if tslEx
        then do
          tsl <- readFile tslPath
          case fromTSLtoTSLSpec tsl of
            Left err -> putStrLn (show err)
            Right spec -> do
              core <- generateCore strixContext spec
              case core of
                NaC -> print "Not unrealizable"
                Unrez s -> putStrLn ("UNREALIZABLE\n\n" ++ tslSpecToString s)
                Unsat s -> putStrLn ("UNSATISFIABLE\n\n" ++ tslSpecToString s)
        else do
          putStrLn $ "File " ++ tslPath ++ " does not exists"
          exitFailure
    _ -> do
      putStrLn "Usage: coregen <tsl-file>"
      exitFailure
-----------------------------------------------------------------------------
