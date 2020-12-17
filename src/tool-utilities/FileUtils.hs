----------------------------------------------------------------------------
-- |
-- Module      :  FileUtils
-- Maintainer  :  Marvin Stenger (Stenger@ProjectJARVIS.de)
--
-- This module implements file utilities that can be used in
-- the executables of tsltools.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
module FileUtils
  ( readContent
  , writeContent
  ) where

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