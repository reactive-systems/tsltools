----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Prints encoded information about inputs, outputs, and terms from a
-- given CFM represented as an AIGER circuit that has been created
-- from  aTSL specification.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils
  ( initEncoding
  )

import PrintUtils
  ( Color(..)
  , ColorIntensity(..)
  , cPutOut
  , cPutOutLn
  , cPutErr
  , cPutErrLn
  )

import TSL
  ( fromCFM
  , symbolTable
  , toCSV
  )

import System.Directory
  ( doesFileExist
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  args <- getArgs

  if length args /= 1 then do
    cPutErr Vivid Yellow "Usage: "
    cPutErrLn Dull White "cfmsym <file>"
    exitFailure
  else do
    exists <- doesFileExist $ head args

    if not exists then do
      cPutErr Vivid Red "File not found: "
      cPutErrLn Vivid White $ head args
      exitFailure
    else do
      str <- readFile $ head args
      case fromCFM str of
        Left err -> invalid (head args) $ show err
        Right m  -> do
          let
            table = toCSV $ symbolTable m
            (h:es) = lines table

          header $ dropLast h
          sep $ dropLast h
          mapM_ (entries . dropLast) es

  where
    invalid file err = do
      cPutOut Vivid Red "invalid: "
      cPutOutLn Vivid White file
      cPutErrLn Dull White err
      exitFailure

    dropLast x = case span (/= ';') $ reverse x of
      (_, ';':y) -> case span (/= ';') y of
        (_, ';':z) -> reverse z
        _          -> reverse y
      _          -> x

    header h = case span (/= ';') h of
      ([], [])     -> return ()
      (rs, [])     -> cPutOutLn Vivid Yellow rs
      (xs, ';':rs) -> cPutOut Vivid Yellow xs >> cPutOut Vivid White ";" >> header rs
      _            -> undefined

    sep h = cPutOutLn Vivid White $ map (\x -> if x == ';' then ';' else '-') h

    entries es = case span (/= ';') es of
      ([], [])     -> return ()
      (rs, [])     -> cPutOutLn Dull White rs
      (xs, ';':rs) -> cPutOut Dull White xs >> cPutOut Vivid White ";" >>
                     entries rs
      _            -> undefined

-----------------------------------------------------------------------------
