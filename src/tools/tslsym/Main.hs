----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Prints the symboltable of a TSL specification.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards
  , LambdaCase
  , ImplicitParams

  #-}

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
  ( Specification(..)
  , fromTSL
  , toCSV
  )

import Config
  ( Configuration(..)
  , parseArguments
  )

import Data.List
  ( isInfixOf
  , partition
  )

import Control.Monad
  ( when
  )

import System.Environment
  ( getArgs
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding

  Configuration{..} <- getArgs >>= parseArguments

  when pHelp $ do
    cPutOutLn Dull White   ""
    cPutOut Vivid Yellow   "Usage:"
    cPutOut Vivid White    " tslsym [OPTIONS]"
    cPutOutLn Dull White   " <file>"
    cPutOutLn Dull White   ""
    cPutOutLn Dull White   "  Prints the symboltable of a TSL specification."
    cPutOutLn Dull White   ""
    cPutOutLn Vivid Yellow "Options:"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -f, --full           "
    cPutOutLn Dull White   "prints the full table, including locally bound definitions"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -n, --no-positions   "
    cPutOutLn Dull White   "removes the 'Position'-column from the table such that"
    cPutOut Dull White     "                       "
    cPutOutLn Dull White   "the resulting table is identical to the one of 'cfmsym'"
    cPutOutLn Dull White   ""
    cPutOut Vivid White    "  -h, --help           "
    cPutOutLn Dull White   "displays this help"
    cPutOutLn Dull White   ""
    cPutOutLn Dull White   "If no input file is given, the input is read from STDIN."
    cPutOutLn Dull White   ""
    exitSuccess

  cnt <- case inputFile of
    Nothing   -> getContents
    Just file -> readFile file

  let ?specFilePath = inputFile

  fromTSL cnt >>= \case
    Left err -> do
      case inputFile of
        Nothing   -> return ()
        Just file -> do
          cPutOut Vivid Red "invalid: "
          cPutOutLn Vivid White file

      cPutErrLn Dull White $ show err
      exitFailure
    Right s -> do
      let
        table = toCSV $ symboltable s
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
