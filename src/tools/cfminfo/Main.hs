----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Returns some statistical numbers of a CFM.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( fromCFM
  , statistics
  )

import System.Directory
  ( doesFileExist
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

import System.FilePath.Posix
  ( takeFileName
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

  case args of
    [file] -> do
      exists <- doesFileExist file

      if not exists then do
        cError Yellow "File not found: "
        cErrorLn White file
        resetColors
        exitFailure
      else do
        str <- readFile file

        case fromCFM str of
          Left err -> invalid file $ show err
          Right cfm  -> do
            let (nI, nO, nP, nF, nC, nV) = statistics cfm
            cPutStr Yellow $ takeFileName file
            cPutStrLn White ":"
            cPutStr White "  inputs:     "
            cPutStrLn White $ show nI
            cPutStr White "  outputs:    "
            cPutStrLn White $ show nO
            cPutStr White "  predicates: "
            cPutStrLn White $ show nP
            cPutStr White "  functions:  "
            cPutStrLn White $ show nF
            cPutStr White "  cells:      "
            cPutStrLn White $ show nC
            cPutStr White "  vertices:   "
            cPutStrLn White $ show nV
            resetColors

    _ -> do
      cError Yellow "Usage: "
      cErrorLn White "cfminfo <file>"
      resetColors
      exitFailure

  where
    invalid file err = do
      cPutStr Red "invalid: "
      cPutStrLn White file
      resetColors
      hPrint stderr err
      hPutStr stderr ""

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
