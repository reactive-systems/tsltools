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
import TSL (fromTSL, toTOML)

import System.Directory (doesFileExist)

import System.FilePath (takeBaseName)

import System.Environment (getArgs)

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hSetSGR
  , setSGR
  )

import System.IO (hPrint, hPutStr, hPutStrLn, stderr)

import GHC.IO.Encoding
  ( setFileSystemEncoding
  , setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

import System.Exit (exitFailure)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  input <- parseArgs
  case input of
    Nothing -> do
      str <- getContents
      let ?specFilePath = Nothing
      res <- fromTSL str
      case res of
        Left err -> do
          cPutStr Red "invalid"
          resetColors
          hPrint stderr err
          exitFailure
        Right s -> putStr $ toTOML "stdin" s
    Just filepath -> do
      exists <- doesFileExist filepath
      if not exists
        then do
          cError Red "File not found: "
          cErrorLn White filepath
          resetColors
          exitFailure
        else do
          str <- readFile filepath
          let ?specFilePath = Just filepath
          res <- fromTSL str
          case res of
            Left err -> do
              cPutStr Red "invalid: "
              cPutStrLn White filepath
              resetColors
              hPrint stderr err
              exitFailure
            Right s -> putStr $ toTOML (takeBaseName filepath) s
  where
    cPutStr c str = do
      setSGR [SetColor Foreground Vivid c]
      putStr str
    cPutStrLn c str = do
      setSGR [SetColor Foreground Vivid c]
      putStrLn str
    cError c str = do
      hSetSGR stderr [SetColor Foreground Vivid c]
      hPutStr stderr str
    cErrorLn c str = do
      hSetSGR stderr [SetColor Foreground Vivid c]
      hPutStrLn stderr str
    resetColors = do
      hSetSGR stderr [Reset]
      setSGR [Reset]
    parseArgs = do
      args <- getArgs
      case args of
        [] -> return Nothing
        x:_ -> return $ Just x
-----------------------------------------------------------------------------
