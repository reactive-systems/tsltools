----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Returns the size of a TSL formula induced by a TSL specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import PrintUtils (Color(..), ColorIntensity(..), cPutOut, cPutOutLn)

import FileUtils (loadTSL)

import TSL (Specification(..), size, toFormula)

import System.FilePath.Posix (takeFileName)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath
            ("tslsize", "Returns the size of a TSL formula induced by a TSL specification.")

  Specification{..} <- loadTSL input

  case input of
    Just file -> do
      cPutOut Vivid Yellow $ takeFileName file
      cPutOutLn Vivid White ":"
    Nothing -> return ()

  cPutOutLn Vivid White $
    "size:     " ++ show (size $ toFormula assumptions guarantees)

-----------------------------------------------------------------------------
