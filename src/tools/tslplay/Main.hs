----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim
--
-- Allows to play against a strategies.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)
import FileUtils (tryReadFile)
import PrintUtils
  ( Color (..),
    ColorIntensity (..),
    cPrintErrLn,
    cPutErr,
    cPutErrLn,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import TSL (simulate)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  args <- getArgs

  case args of
    [tsl, cfm] -> do
      spec <- tryReadFile tsl
      strat <- tryReadFile cfm

      simulate (Just tsl) spec strat >>= \case
        Right simulate -> do
          simulate
          exitSuccess
        Left err -> do
          cPrintErrLn Vivid Red err
          exitFailure
    _ -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "tslplay <tsl-file> <cfm-file>"
      exitFailure

-----------------------------------------------------------------------------
