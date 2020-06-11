----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Inlines all imports of a TSL specification
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------
import TSL (fromTSL, toTSL)

import System.Directory (doesFileExist)

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
  args <- getArgs
  if null args
    then getContents >>= fromTSL >>= \case
           Left err -> do
             cPutStrLn Red "invalid"
             resetColors
             hPrint stderr err
             exitFailure
           Right s -> putStr $ toTSL s
    else do
      exists <- doesFileExist $ head args
      if not exists
        then do
          cError Red "File not found: "
          cErrorLn White $ head args
          resetColors
          exitFailure
        else readFile (head args) >>= fromTSL >>= \case
               Left err -> do
                 cPutStr Red "invalid: "
                 cPutStrLn White $ head args
                 resetColors
                 hPrint stderr err
                 exitFailure
               Right s -> putStr $ toTSL s
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
-----------------------------------------------------------------------------
