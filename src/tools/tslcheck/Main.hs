----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Checks TSL specifications to be in a valid format.
--
-----------------------------------------------------------------------------
module Main
  ( main
  ) where

-----------------------------------------------------------------------------
import TSL (fromTSL)

import System.Directory (doesDirectoryExist, doesFileExist)

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , hSetSGR
  , setSGR
  )

import System.Environment (getArgs)

import System.IO (hPrint, hPutStr, hPutStrLn, stderr)

import GHC.IO.Encoding
  ( setFileSystemEncoding
  , setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

import System.Exit (exitFailure, exitSuccess)

-----------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  args <- getArgs
  if null args
  then do
    valid <- checkStdIn
    if valid
    then exitSuccess
    else exitFailure
  else do
    xs <- mapM checkFile args

    if and xs
    then exitSuccess
    else exitFailure

  where
    checkFile file = do
      exists <- doesFileExist file
      if not exists
        then do
          dir <- doesDirectoryExist file
          if dir
            then do
              cPutStr Yellow "directory: "
              cPutStr White file
              cPutStrLn Yellow " (skipping)"
            else do
              cError Red "Not found: "
              cErrorLn White file
          resetColors
          return False
        else do
          str <- readFile file
          case fromTSL str of
            Left err -> do
              cPutStr Red "invalid: "
              cPutStrLn White file
              resetColors
              hPrint stderr err
              hPutStrLn stderr ""
              return False
            Right _ -> do
              cPutStr Green "valid: "
              cPutStrLn White file
              resetColors
              return True

    checkStdIn = do
        str <- getContents
        case fromTSL str of
          Left err -> do
            cPutStr Red "invalid"
            resetColors
            hPrint stderr err
            hPutStrLn stderr ""
            return False

          Right _  -> do
            cPutStr Green "valid"
            resetColors
            return True

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
    resetColors = hSetSGR stderr [Reset]

-----------------------------------------------------------------------------
