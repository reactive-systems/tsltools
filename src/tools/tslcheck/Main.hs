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

import TSL
  ( fromTSL
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
  then do
    cError Yellow "Usage: "
    cErrorLn White "tslcheck <files>"
    resetColors
    exitFailure
  else
    mapM_ checkFile args

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
          cError Red "Not found: "
          cErrorLn White file

        resetColors

      else do
        str <- readFile file
        case fromTSL str of
          Left err -> do
            cPutStr Red "invalid: "
            cPutStrLn White file
            resetColors
            hPrint stderr err
            hPutStrLn stderr ""
          Right _  -> do
            cPutStr Green "valid: "
            cPutStrLn White file
            resetColors

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
