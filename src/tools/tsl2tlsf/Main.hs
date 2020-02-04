----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Transforms TSL specifications into TLSF specifications.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

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

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , setSGR
  , hSetSGR
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
      str <- getContents
      case fromTSL str of
        Left err -> do
          cPutStrLn Red "invalid"
          resetColors
          hPrint stderr err
          exitFailure
        Right s  ->
          putStr $ toTLSF "stdin" s
  else do
    exists <- doesFileExist $ head args

    if not exists then do
      cError Red "File not found: "
      cErrorLn White $ head args
      resetColors
      exitFailure
    else do
      str <- readFile $ head args
      case fromTSL str of
        Left err -> do
          cPutStr Red "invalid: "
          cPutStrLn White $ head args
          resetColors
          hPrint stderr err
          exitFailure
        Right s  ->
          putStr $ toTLSF (takeBaseName (head args)) s

  where
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

    resetColors = do
      hSetSGR stderr [ Reset ]
      setSGR [ Reset ]

-----------------------------------------------------------------------------
