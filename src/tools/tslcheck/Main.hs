----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Checks TSL specifications to be in a valid format.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
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
  ( fromTSL
  )

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

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
      let ?specFilePath = Just file
      exists <- doesFileExist file
      if not exists
        then do
          dir <- doesDirectoryExist file
          if dir
            then do
              cPutOut Vivid Yellow "directory: "
              cPutOut Vivid White file
              cPutOutLn Vivid Yellow " (skipping)"
            else do
              cPutErr Vivid Red "Not found: "
              cPutErrLn Vivid White file
          return False
        else
          readFile file >>= fromTSL >>= \case
            Left err -> do
              cPutOut Vivid Red "invalid: "
              cPutOutLn Vivid White file
              putErrLn err
              putErr ""
              return False
            Right _ -> do
              cPutOut Vivid Green "valid: "
              cPutOutLn Vivid White file
              return True

    checkStdIn =
      let ?specFilePath = Nothing in
      getContents >>= fromTSL >>= \case
        Left err -> do
          cPutOut Vivid Red "invalid"
          putErrLn err
          putErr ""
          return False
        Right _  -> do
          cPutOut Vivid Green "valid"
          return True

-----------------------------------------------------------------------------
