----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Gideon Geier
--
-- Splits TSL specifications into many TSL specifications (if possible).
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import FileUtils (loadTSL)

import TSL (split, toTSL)

import System.Directory (getCurrentDirectory)

import System.FilePath (takeBaseName, (<.>), (</>))

import Data.Maybe (fromMaybe)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath
            ("tslsplit", "Splits TSL specifications into many TSL specifications (if possible).")

  spec <- loadTSL input

  path <- getCurrentDirectory

  let
    specs = split spec

    filepathN n =
      path </> (fromMaybe "SPLIT" $ takeBaseName <$> input) ++
      "_" ++ show n <.> "tsl"

  mapM_
    (\(s,n) -> writeFile (filepathN n) (toTSL s))
    (zip specs [1::Int,2..])

-----------------------------------------------------------------------------
