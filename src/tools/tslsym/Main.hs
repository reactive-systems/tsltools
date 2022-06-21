----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein
--
-- Prints the symboltable of a TSL specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)

import PrintUtils (Color(..), ColorIntensity(..), cPutOut, cPutOutLn)

import FileUtils (loadTSL)

import TSL (Specification(..), toCSV)

import Config (Configuration(..), parseArguments)

import Data.List (isInfixOf, partition)

import Control.Monad (when)

import Debug.Trace (trace)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{input, fullTable, noPositions} <- parseArguments

  spec <- loadTSL input
  let
    table = toCSV $ symboltable $ trace (show spec) spec
    (is,ts') = partition (isInfixOf "internal") es
    (h':es) = lines table

    (h, ts, upd)
      | fullTable =
          (h', ts', if noPositions then rmSLast else id)
      | otherwise =
          let (h'':ts'') = rmSpaces [] (h':ts')
          in (h'',ts'', if noPositions then rmLast . rmLast else rmLast)

  header $ upd h
  sep $ upd h
  mapM_ (entries . upd) ts

  when fullTable $ do
    sep $ upd h
    mapM_ (entries . upd) is

  where
    rmSpaces a xs =
      let
        (is,rs) = unzip $ map (break (== ';')) xs
        n = minimum $ map (length . takeWhile (== ' ') . reverse) is
      in case head rs of
        "" ->
          let
            is' =
              if n > 0
              then map (take (length (head is) - n + 1)) is
              else is

          in
            foldl (zipWith (\x y -> y ++ ';' : x)) is' a
        _  ->
          let
            is' =
              if n > 1
              then map (take (length (head is) - n + 1)) is
              else is
          in
            rmSpaces (is' : a) $ map (\(';':zs) -> zs) rs

    rmLast xs =
      let (_, ';':_:xr) = break (== ';') $ reverse xs
      in reverse xr

    rmSLast xs =
      let
        (l, ';':xr) = break (== ';') $ reverse xs
        (_, ';':yr) = break (== ';') xr
      in
        reverse $ l ++ ';' : yr

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
