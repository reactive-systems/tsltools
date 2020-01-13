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
import CoreGen.ToolCall (generateCoreFromFile)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [tsl] -> do
      tslEx <- doesFileExist tsl
      if tslEx
        then generateCoreFromFile tsl
        else do
          putStrLn $ "File " ++ tsl ++ " does not exists"
          exitFailure
    _ -> do
      putStrLn "Usage: coregen <tsl-file>"
      exitFailure
-----------------------------------------------------------------------------
