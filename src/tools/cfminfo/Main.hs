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

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErr
  , printErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromCFM
  , statistics
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
        str <- readFile file

        case fromCFM str of
          Left err -> invalid file $ show err
          Right cfm  -> do
            let (nI, nO, nP, nF, nC, nV) = statistics cfm
            cPutOut Vivid Yellow $ takeFileName file
            cPutOutLn Vivid White ":"
            cPutOut Vivid White "  inputs:     "
            cPutOutLn Vivid White $ show nI
            cPutOut Vivid White "  outputs:    "
            cPutOutLn Vivid White $ show nO
            cPutOut Vivid White "  predicates: "
            cPutOutLn Vivid White $ show nP
            cPutOut Vivid White "  functions:  "
            cPutOutLn Vivid White $ show nF
            cPutOut Vivid White "  cells:      "
            cPutOutLn Vivid White $ show nC
            cPutOut Vivid White "  vertices:   "
            cPutOutLn Vivid White $ show nV

    _ -> do
      cPutErr Vivid Yellow "Usage: "
      cPutErrLn Vivid White "cfminfo <file>"
      exitFailure

  where
    invalid file err = do
      cPutOut Vivid Red "invalid: "
      cPutOutLn Vivid White file
      printErrLn err

-----------------------------------------------------------------------------
