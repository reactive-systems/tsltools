----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Returns some statistical numbers of a CFM.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    NamedFieldPuns

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import ArgParseUtils
  ( parseMaybeFilePath
  )

import FileUtils
  ( loadCFM
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  )

import TSL
  ( statistics
  )

import System.FilePath.Posix
  ( takeFileName
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath "cfminfo"

  cfm <- loadCFM input

  let (nI, nO, nP, nF, nC, nV) = statistics cfm

  case input of
    Just file -> do
      cPutOut Vivid Yellow $ takeFileName file
      cPutOutLn Vivid White ":"
    Nothing -> return ()

  cPutOutLn Vivid White $ unlines
    [ "inputs:     " ++ show nI
    , "outputs:    " ++ show nO
    , "predicates: " ++ show nP
    , "functions:  " ++ show nF
    , "cells:      " ++ show nC
    , "vertices:   " ++ show nV
    ]

-----------------------------------------------------------------------------
