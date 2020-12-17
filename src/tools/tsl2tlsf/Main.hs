----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms TSL specifications into TLSF specifications.
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
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromTSL
  , toTLSF
  )

import System.Directory
  ( doesFileExist
  )

import System.FilePath
  ( takeBaseName
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
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  args <- getArgs
  if null args
  then do
    let ?specFilePath = Nothing 
    getContents >>= fromTSL >>= \case
      Left err -> do
        cPutOutLn Vivid Red "invalid"
        putErrLn err
        exitFailure
      Right s  ->
        putStr $ toTLSF "stdin" s
  else do
    let path = head args
    let ?specFilePath = Just path

    exists <- doesFileExist path

    if not exists then do
      cPutErr Vivid Red "File not found: "
      cPutErrLn Vivid White path
      exitFailure
    else
      readFile path >>= fromTSL >>= \case
        Left err -> do
          cPutOut Vivid Red "invalid: "
          cPutOutLn Vivid White path
          putErrLn err
          exitFailure
        Right s  ->
          putStr $ toTLSF (takeBaseName path) s

-----------------------------------------------------------------------------
