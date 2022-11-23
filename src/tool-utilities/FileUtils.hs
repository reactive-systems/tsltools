----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stenger
--
-- This module implements file utilities that can be used in
-- the executables of tsltools.
module FileUtils
  ( checkFile,
    tryReadFile,
    tryReadContent,
    readContent,
    writeContent,
    loadTSL,
    loadTSLMT,
    loadCFM,
  )
where

-----------------------------------------------------------------------------

import Control.Monad (unless)
import PrintUtils
  ( Color (..),
    ColorIntensity (..),
    cPutErr,
    cPutErrLn,
    cPutMessageInput,
    printErrLn,
  )
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import TSL
  ( CFM,
    Error,
    Specification,
    Theory,
    fromCFM,
    fromTSL,
    readTheory,
    unwrap,
  )

-----------------------------------------------------------------------------

-- | Checks if given FilePath belongs to an existing file.
-- If the file does not exists, function exits with an error mesage.
checkFile :: FilePath -> IO ()
checkFile file =
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

-- | helper function returning valid result or exiting with an error message
rightOrInvalidInput :: Maybe FilePath -> Either Error a -> IO a
rightOrInvalidInput input = \case
  Right r -> return r
  Left err -> do
    cPutMessageInput Red "invalid" input
    printErrLn err
    exitFailure

-----------------------------------------------------------------------------

-- | 'loadTSL' is a helper function which loads and parses a TSL file and
-- if this is not possible outputs a respective error on the command line
-- and exits
loadTSL :: Maybe FilePath -> IO Specification
loadTSL input =
  tryReadContent input
    >>= fromTSL input
    >>= rightOrInvalidInput input

-----------------------------------------------------------------------------

-- | 'loadTSLMT' is a helper function which loads and parses a TSLMT file and
-- if this is not possible outputs a respective error on the command line
-- and exits
loadTSLMT :: Maybe FilePath -> IO (Theory, Specification, String)
loadTSLMT input = do
  content <- tryReadContent input
  let linesList = lines content
      theory = readTheory $ head linesList
      specStr = unlines $ tail linesList -- FIXME: computationally wasteful
  tslmt <- fromTSL input specStr
  unwrap $ (,,specStr) <$> theory <*> tslmt

-----------------------------------------------------------------------------

-- | 'loadCFM' is a helper function which loads and parses a CFM file and
-- if this is not possible outputs a respective error on the command line
-- and exits
loadCFM :: Maybe FilePath -> IO CFM
loadCFM input = do
  content <- tryReadContent input
  let e = fromCFM content
  rightOrInvalidInput input e
