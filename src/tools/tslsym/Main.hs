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

  #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------

import TSL
  ( Specification(..)
  , fromTSL
  , toCSV
  )

import Config
  ( Configuration(..)
  , parseArguments
  , cPutStrLn
  , cPutStr
  , cErrStrLn
  , resetColors
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

import System.Console.ANSI
  ( ColorIntensity(..)
  , Color(..)
  )

import GHC.IO.Encoding
  ( utf8
  , setLocaleEncoding
  , setFileSystemEncoding
  , setForeignEncoding
  )

import System.Exit
  ( exitFailure
  , exitSuccess
  )

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  Configuration{..} <- getArgs >>= parseArguments

  when pHelp $ do
    cPutStrLn Dull White   ""
    cPutStr Vivid Yellow   "Usage:"
    cPutStr Vivid White    " tslsym [OPTIONS]"
    cPutStrLn Dull White   " <file>"
    cPutStrLn Dull White   ""
    cPutStrLn Dull White   "  Prints the symboltable of a TSL specification."
    cPutStrLn Dull White   ""
    cPutStrLn Vivid Yellow "Options:"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -f, --full           "
    cPutStrLn Dull White   "prints the full table, including locally bound definitions"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -n, --no-positions   "
    cPutStrLn Dull White   "removes the 'Position'-column from the table such that"
    cPutStr Dull White     "                       "
    cPutStrLn Dull White   "the resulting table is identical to the one of 'cfmsym'"
    cPutStrLn Dull White   ""
    cPutStr Vivid White    "  -h, --help           "
    cPutStrLn Dull White   "displays this help"
    cPutStrLn Dull White   ""
    cPutStrLn Dull White   "If no input file is given, the input is read from STDIN."
    cPutStrLn Dull White   ""
    resetColors
    exitSuccess

  cnt <- case inputFile of
    Nothing   -> getContents
    Just file -> readFile file

  case fromTSL cnt of
    Left err -> do
      case inputFile of
        Nothing   -> return ()
        Just file -> do
          cPutStr Vivid Red "invalid: "
          cPutStrLn Vivid White file

      cErrStrLn Dull White $ show err
      resetColors
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

      resetColors

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
      (rs, [])     -> cPutStrLn Vivid Yellow rs
      (xs, ';':rs) -> cPutStr Vivid Yellow xs >> cPutStr Vivid White ";" >> header rs
      _            -> undefined

    sep h = cPutStrLn Vivid White $ map (\x -> if x == ';' then ';' else '-') h

    entries es = case span (/= ';') es of
      ([], [])     -> return ()
      (rs, [])     -> cPutStrLn Dull White rs
      (xs, ';':rs) -> cPutStr Dull White xs >> cPutStr Vivid White ";" >>
                     entries rs
      _            -> undefined

-----------------------------------------------------------------------------
