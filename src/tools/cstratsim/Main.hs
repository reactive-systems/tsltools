----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Allows to play against a counterstategy
--
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import Simulator.CLI (simulate)

import System.Directory (doesFileExist)

import System.Environment (getArgs)

import System.Exit (exitFailure)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  -- TODO no encoding initialization needed?
  args <- getArgs
  case args of
    [tsl, aag] -> do
      tslEx <- doesFileExist tsl
      aagEx <- doesFileExist aag
      if tslEx
        then if aagEx
               then simulate tsl aag
               else do
                 putStrLn $ "File " ++ aag ++ " does not exists"
                 exitFailure
        else do
          putStrLn $ "File " ++ tsl ++ " does not exists"
          exitFailure
    _ -> do
      putStrLn "Usage: cstartsim <tsl-file> <aag-file>"
      exitFailure
-----------------------------------------------------------------------------
