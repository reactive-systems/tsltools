----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Returns the size of a TSL formula induced by a TSL specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , LambdaCase
  , ImplicitParams

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( Specification(..)
  , fromTSL
  , size
  , toFormula
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
        let ?specFilePath = Just file
        readFile file >>= fromTSL >>= \case
          Left err -> do
            cPutStr Red "invalid: "
            cPutStrLn White file
            resetColors
            hPrint stderr err
            hPutStrLn stderr ""
          Right Specification{..}  -> do
            cPutStr Yellow $ takeFileName file
            cPutStrLn White ":"
            cPutStr White "  size:       "
            cPutStrLn White $ show $ size $ toFormula assumptions guarantees
            resetColors
    _ -> do
      cError Yellow "Usage: "
      cErrorLn White "tslsize <file>"
      resetColors
      exitFailure

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

    resetColors =
      hSetSGR stderr [ Reset ]

-----------------------------------------------------------------------------
