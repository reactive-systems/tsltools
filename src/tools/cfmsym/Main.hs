----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Prints encoded information about inputs, outputs, and terms from a
-- given CFM represented as an AIGER circuit that has been created
-- from a TSL specification.
--
-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import ArgParseUtils (parseMaybeFilePath)

import FileUtils (loadCFM)

import PrintUtils (Color(..), ColorIntensity(..), cPutOut, cPutOutLn)

import TSL (symbolTable, toCSV)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  input <- parseMaybeFilePath
            ("cfmsym", "Prints encoded information about inputs, outputs, and terms from a given CFM represented as an AIGER circuit that has been created from a TSL specification.")

  m <- loadCFM input

  let
    table = toCSV $ symbolTable m
    (h:es) = lines table

  header $ dropLast h
  sep $ dropLast h
  mapM_ (entries . dropLast) es

  where
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
