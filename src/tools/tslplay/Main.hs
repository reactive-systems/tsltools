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

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( simulate
  )

import Control.Monad
  ( unless
  )

import System.Directory
  ( doesFileExist
  )

import System.Environment
  ( getArgs
  )

import GHC.IO.Encoding
  ( utf8
  , setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  args <- getArgs
  case args of
    [tsl, cfm] -> do
      (>>=) (doesFileExist tsl) $ flip unless $ do
        cPutErr Vivid Red "Not found: "
        cPutErrLn Vivid White tsl
        exitFailure

      spec <- readFile tsl
      let ?specFilePath = Just tsl

      (>>=) (doesFileExist cfm) $ flip unless $ do
        cPutErr Vivid Red "Not found: "
        cPutErrLn Vivid White cfm
        exitFailure

      strat <- readFile cfm

      simulate spec strat >>= \case
        Right simulate -> do
          simulate
          exitSuccess
        Left err       -> do
          cPutErr Vivid Red $ show err
          exitFailure

    _ -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "tslplay <tsl-file> <cfm-file>"
      exitFailure

-----------------------------------------------------------------------------
