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
  ( fromCFM
  )

import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  )

import System.Environment
  ( getArgs
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

  if null args
  then do
    cPutErr Vivid Yellow "Usage: "
    cPutErrLn Vivid White "cfmcheck <files>"
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
          cPutOut Vivid Yellow "directory: "
          cPutOut Vivid White file
          cPutOutLn Vivid Yellow " (skipping)"
        else do
          cPutErr Vivid Red "File not found: "
          cPutErrLn Vivid White file

      else do
        str <- readFile file

        case fromCFM str of
          Left err -> invalid file $ show err
          Right _  -> do
            cPutOut Vivid Green "valid: "
            cPutOutLn Vivid White file

    invalid file err = do
      cPutOut Vivid Red "invalid: "
      cPutOutLn Vivid White file
      putErrLn err
      putErr ""
