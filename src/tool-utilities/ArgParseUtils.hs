----------------------------------------------------------------------------
-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stegner (Stenger@ProjectJARVIS.de)
--
-- This module implements command line argument parsing utilities that can
-- be used in the executables of tsltools.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    LambdaCase
  , RecordWildCards
  , NamedFieldPuns

  #-}

-----------------------------------------------------------------------------

module ArgParseUtils
  ( parseMaybeFilePath
  )
  where

-----------------------------------------------------------------------------

import FileUtils
  ( checkFile
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , putErrLn
  , cPutErrLn
  )

-----------------------------------------------------------------------------

parseMaybeFilePath :: String -> IO (Maybe FilePath)
parseMaybeFilePath toolName = do
  args <- getArgs
  case args of
    [] -> return Nothing
    [path] -> do
      checkFile path
      return $ Just path
    _ -> do
      cPutErrLn Vivid Red "Invalid argument given"
      putErrLn ""
      putErrLn $ unlines
        [ "Usage:"
        , "  " ++ toolName ++ " [FILE]"
        , ""
        , "  STDIN is used, if no FILE given"
        ]
      exitFailure
