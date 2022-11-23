----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Returns some statistical numbers of a CFM.
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import ArgParseUtils (parseMaybeFilePath)
import EncodingUtils (initEncoding)
import FileUtils (loadCFM)
import PrintUtils (Color (..), ColorIntensity (..), cPutOut, cPutOutLn)
import System.FilePath.Posix (takeFileName)
import TSL (statistics)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding

  input <-
    parseMaybeFilePath
      ("cfminfo", "Returns some statistical numbers of a CFM.")

  cfm <- loadCFM input

  let (nI, nO, nP, nF, nC, nV) = statistics cfm

  case input of
    Just file -> do
      cPutOut Vivid Yellow $ takeFileName file
      cPutOutLn Vivid White ":"
    Nothing -> return ()

  cPutOutLn Vivid White $
    unlines
      [ "inputs:     " ++ show nI,
        "outputs:    " ++ show nO,
        "predicates: " ++ show nP,
        "functions:  " ++ show nF,
        "cells:      " ++ show nC,
        "vertices:   " ++ show nV
      ]

-----------------------------------------------------------------------------
