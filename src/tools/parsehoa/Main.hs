{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Main
-- Maintainer  :  Mark Santolucito
--
-- Generates code from a HOA file generated from a TSL spec
module Main
  ( main,
  )
where

-----------------------------------------------------------------------------

import EncodingUtils (initEncoding)
import Hanoi (HOA (..), parse, printHOA)
import System.Environment
import TSL (tlsfToTslTerm)

-----------------------------------------------------------------------------

main ::
  IO ()
main = do
  initEncoding
  args <- getArgs
  c <- readFile $ head args
  let hoa = parse c
  putStrLn $ either id (printHOA . translateEdges) hoa

translateEdges :: HOA -> HOA
translateEdges hoa@HOA {..} =
  hoa {atomicPropositionName = tlsfToTslTerm . atomicPropositionName}
