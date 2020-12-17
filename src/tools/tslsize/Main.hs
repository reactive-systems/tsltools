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
  ( Specification(..)
  , fromTSL
  , size
  , toFormula
  )

import System.Directory
  ( doesFileExist
  )

import System.Environment
  ( getArgs
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
  initEncoding

  args <- getArgs
  case args of
    [file] -> do
      exists <- doesFileExist file

      if not exists then do
        cPutErr Vivid Yellow "File not found: "
        cPutErrLn Vivid White file
        exitFailure
      else do
        let ?specFilePath = Just file
        readFile file >>= fromTSL >>= \case
          Left err -> do
            cPutOut Vivid Red "invalid: "
            cPutOutLn Vivid White file
            putErrLn err
            putErr ""
          Right Specification{..}  -> do
            cPutOut Vivid Yellow $ takeFileName file
            cPutOutLn Vivid White ":"
            cPutOut Vivid White "  size:       "
            cPutOutLn Vivid White $ show $ size $ toFormula assumptions guarantees
    _ -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "tslsize <file>"
      exitFailure

-----------------------------------------------------------------------------
