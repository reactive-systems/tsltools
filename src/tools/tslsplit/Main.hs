----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Gideon Geier
--
-- Splits TSL specifications into many TSL specifications (if possible).
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import Config (Configuration (..), parseArguments)
import Data.Maybe (fromMaybe)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName, (<.>), (</>))
import TSL (split, splitAssumptions, toTSL)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  Configuration {input, assumptions} <- parseArguments

  spec <- loadTSL input
  path <- getCurrentDirectory

  let specs
        | assumptions = splitAssumptions spec
        | otherwise = split spec

      identifier
        | assumptions = "a"
        | otherwise = "s"

      filepathN n i =
        path
          </> takeBaseName (fromMaybe "TSLSplitFromSTDIN" input)
          ++ "_"
          ++ i
          ++ show n <.> "tsl"

  mapM_
    (\(s, n) -> writeFile (filepathN n identifier) (toTSL s))
    (zip specs [1 :: Int, 2 ..])

-----------------------------------------------------------------------------
