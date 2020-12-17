----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, ImplicitParams #-}

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
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL (fromTSL, toTOML)

import System.Directory (doesFileExist)

import System.FilePath (takeBaseName)

import System.Environment (getArgs)

import System.Exit (exitFailure)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  initEncoding

  input <- parseArgs
  case input of
    Nothing -> do
      str <- getContents
      let ?specFilePath = Nothing
      res <- fromTSL str
      case res of
        Left err -> do
          cPutOut Vivid Red "invalid"
          putErrLn err
          exitFailure
        Right s -> putStr $ toTOML "stdin" s
    Just filepath -> do
      exists <- doesFileExist filepath
      if not exists
        then do
          cPutErr Vivid Red "File not found: "
          cPutErrLn Vivid White filepath
          exitFailure
        else do
          str <- readFile filepath
          let ?specFilePath = Just filepath
          res <- fromTSL str
          case res of
            Left err -> do
              cPutOut Vivid Red "invalid: "
              cPutOutLn Vivid White filepath
              putErrLn err
              exitFailure
            Right s -> putStr $ toTOML (takeBaseName filepath) s
  where
    parseArgs = do
      args <- getArgs
      case args of
        [] -> return Nothing
        x:_ -> return $ Just x
-----------------------------------------------------------------------------
