----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Returns the size of a TSL formula induced by a TSL specification.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import ArgParseUtils (parseMaybeFilePath)
import EncodingUtils (initEncoding)
import FileUtils (loadTSL)
import PrintUtils (Color (..), ColorIntensity (..), cPutOut, cPutOutLn)
import System.FilePath.Posix (takeFileName)
import TSL (Specification (..), size, toFormula)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  input <-
    parseMaybeFilePath
      ("tslsize", "Returns the size of a TSL formula induced by a TSL specification.")

  Specification {..} <- loadTSL input

  case input of
    Just file -> do
      cPutOut Vivid Yellow $ takeFileName file
      cPutOutLn Vivid White ":"
    Nothing -> return ()

  cPutOutLn Vivid White $
    "size:     " ++ show (size $ toFormula assumptions guarantees)

-----------------------------------------------------------------------------
