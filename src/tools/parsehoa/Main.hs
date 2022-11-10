----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
--
-----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

-----------------------------------------------------------------------------

module Main
  ( main
  ) where

-----------------------------------------------------------------------------


import EncodingUtils (initEncoding)

import TSL (tlsfToTslTerm)

import Hanoi 
  ( HOA(..)
  , parse
  )
import System.Environment

import Hanoi (printHOA)

-----------------------------------------------------------------------------

main
  :: IO ()

main = do
  initEncoding
  args <- getArgs
  c <- readFile $ head args
  let hoa = parse c
  putStrLn $ either id (printHOA. translateEdges) hoa

translateEdges :: HOA -> HOA
translateEdges hoa@HOA {..} =
  hoa {atomicPropositionName = tlsfToTslTerm . atomicPropositionName} 
