----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Allows to play against a stategies.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , ImplicitParams

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  , cPrintErrLn
  )

import FileUtils
  ( tryReadFile
  )

import TSL
  ( simulate
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  args <- getArgs
  case args of
    [tsl, cfm] -> do
      spec <- tryReadFile tsl
      let ?specFilePath = Just tsl

      strat <- tryReadFile cfm

      simulate spec strat >>= \case
        Right simulate -> do
          simulate
          exitSuccess
        Left err       -> do
          cPrintErrLn Vivid Red err
          exitFailure

    _ -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "tslplay <tsl-file> <cfm-file>"
      exitFailure

-----------------------------------------------------------------------------
