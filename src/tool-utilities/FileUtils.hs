----------------------------------------------------------------------------
-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- This module implements file utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE

    LambdaCase

#-}

-----------------------------------------------------------------------------
module FileUtils
  ( checkFile
  , tryReadFile
  , tryReadContent
  , readContent
  , writeContent
  , tryLoadTSL
  , tryLoadCFM
  ) where


-----------------------------------------------------------------------------

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , printErrLn
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( Specification
  , CFM
  , fromTSL
  , fromCFM
  )

import Control.Monad
  ( unless
  )

import System.Directory
  ( doesFileExist
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------
-- | Checks if given FilePath belongs to an existing file.
-- If the file does not exists, function exits with an error mesage.
checkFile :: FilePath -> IO ()
checkFile file = do
  (>>=) (doesFileExist file) $ flip unless $ do
    cPutErr Vivid Red "Not found: "
    cPutErrLn Vivid White file
    exitFailure

-----------------------------------------------------------------------------
-- | Tries to read all content from given file.
-- If the file does not exists, function exits with an error mesage.
tryReadFile :: FilePath -> IO String
tryReadFile file = do
  checkFile file
  readFile file

-----------------------------------------------------------------------------
-- | Tries to read all content from either given file or STDIN.
-- If the file does not exists, function exits with an error mesage.
tryReadContent :: Maybe FilePath -> IO String
tryReadContent Nothing = getContents
tryReadContent (Just file) = do
  checkFile file
  readFile file

-----------------------------------------------------------------------------
-- | Reads all content either from given file or STDIN
readContent :: Maybe FilePath -> IO String
readContent Nothing = getContents
readContent (Just file) = readFile file

-----------------------------------------------------------------------------
-- | Writes content either to given file or STDOUT
writeContent :: Maybe FilePath -> String -> IO ()
writeContent Nothing = putStrLn
writeContent (Just file) = writeFile file


-----------------------------------------------------------------------------
printErrMessage :: Show a => Maybe FilePath -> a -> IO ()
printErrMessage input err = do
  case input of
    Nothing -> do
      cPutOutLn Vivid Red "invalid:"
    Just file -> do
      cPutOut Vivid Red "invalid: "
      cPutOutLn Vivid White file
  printErrLn err

-----------------------------------------------------------------------------
-- | 'tryLoadTSL' is a helper function which loads and parses a TSL file and
-- if this is not possible outputs a respective error on the command line
-- and exits
tryLoadTSL :: Maybe FilePath -> IO Specification
tryLoadTSL input = do
  content <- tryReadContent input
  fromTSL input content
  >>= \case 
    Left err -> do
      printErrMessage input err
      exitFailure
    Right spec -> return spec

-----------------------------------------------------------------------------
-- | 'tryLoadCFM' is a helper function which loads and parses a CFM file and
-- if this is not possible outputs a respective error on the command line
-- and exits
tryLoadCFM :: Maybe FilePath -> IO CFM
tryLoadCFM input = do
  content <- tryReadContent input
  case fromCFM content of
    Left err -> do
      printErrMessage input err
      exitFailure
    Right cfm -> return cfm
