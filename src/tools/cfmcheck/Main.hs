----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Checks for correct labels and structure of a CFM represented as an
-- AIGER circuit, which initally has been created from a TSL
-- specification.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( fromCFM
  )

import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  )

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , setSGR
  , hSetSGR
  )

import System.Environment
  ( getArgs
  )

import System.IO
  ( stderr
  , hPrint
  , hPutStr
  , hPutStrLn
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
  then mapM_ checkFile args
  else do
    cError Yellow "Usage: "
    cErrorLn White "cfmcheck <files>"
    resetColors
    exitFailure

  where
    checkFile file = do
      exists <- doesFileExist file

      if not exists then do
        dir <- doesDirectoryExist file

        if dir
        then do
          cPutStr Yellow "directory: "
          cPutStr White file
          cPutStrLn Yellow " (skipping)"
        else do
          cError Red "File not found: "
          cErrorLn White file

        resetColors
      else do
        str <- readFile file

        case fromCFM str of
          Left err -> invalid file $ show err
          Right _  -> do
            cPutStr Green "valid: "
            cPutStrLn White file
            resetColors

    invalid file err = do
      cPutStr Red "invalid: "
      cPutStrLn White file
      resetColors
      hPrint stderr err
      hPutStrLn stderr ""

    cPutStr c str = do
      setSGR [ SetColor Foreground Vivid c ]
      putStr str

    cPutStrLn c str = do
      setSGR [ SetColor Foreground Vivid c ]
      putStrLn str

    cError c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStr stderr str

    cErrorLn c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStrLn stderr str

    resetColors =
      hSetSGR stderr [ Reset ]

-----------------------------------------------------------------------------
