----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Marvin Stenger (klein@react.uni-saarland.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    ImplicitParams

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
  , putErr
  , putErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromTSL
  , toTSL
  )

import System.Directory
  ( doesFileExist
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseArgs

  case input of
    Nothing -> do
      let ?specFilePath = Nothing
      str <- getContents
      res <- fromTSL str
      case res of
        Left err -> do
          cPutOut Vivid Red "invalid"
          putErrLn err
          exitFailure
        Right s  ->
          putStr $ toTSL s
    Just filepath -> do
      let ?specFilePath = Just filepath
      exists <- doesFileExist filepath

      if not exists then do
        cPutErr Vivid Red "File not found: "
        cPutErrLn Vivid White filepath
        exitFailure
      else do
        str <- readFile filepath
        res <- fromTSL str
        case res of
          Left err -> do
            cPutOut Vivid Red "invalid: "
            cPutOutLn Vivid White filepath
            putErrLn err
            exitFailure
          Right s  ->
            putStr $ toTSL s

  where
    parseArgs = do
      args <- getArgs
      case args of
        []  -> return Nothing
        x:_ -> return $ Just x
    
-----------------------------------------------------------------------------
