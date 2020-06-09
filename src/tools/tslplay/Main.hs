----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Allows to play against a stategies.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( simulate
  )

import Control.Monad
  ( unless
  )

import System.Directory
  ( doesFileExist
  )

import System.Console.ANSI
  ( SGR(..)
  , ConsoleLayer(..)
  , ColorIntensity(..)
  , Color(..)
  , hSetSGR
  )

import System.Environment
  ( getArgs
  )

import System.IO
  ( stderr
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
  , exitSuccess
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
    [tsl, cfm] -> do
      (>>=) (doesFileExist tsl) $ flip unless $ do
        cError Red "Not found: "
        cErrorLn White tsl
        resetColors
        exitFailure

      spec <- readFile tsl

      (>>=) (doesFileExist cfm) $ flip unless $ do
        cError Red "Not found: "
        cErrorLn White cfm
        resetColors
        exitFailure

      strat <- readFile cfm

      simulate spec strat >>= \case
        Right simulate -> do
          simulate
          exitSuccess
        Left err       -> do
          cError Red $ show err
          resetColors
          exitFailure

    _ -> do
      cError Yellow "Usage: "
      cErrorLn White "tslplay <tsl-file> <cfm-file>"
      resetColors
      exitFailure

  where
    cError c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStr stderr str

    cErrorLn c str = do
      hSetSGR stderr [ SetColor Foreground Vivid c ]
      hPutStrLn stderr str

    resetColors =
      hSetSGR stderr [ Reset ]

-----------------------------------------------------------------------------
