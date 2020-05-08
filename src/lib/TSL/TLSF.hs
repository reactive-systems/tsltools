-----------------------------------------------------------------------------
-- |
-- Module      :  TLSF
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- TLSF writer, which transforms a TSL formula into TLSF.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    RecordWildCards

  #-}

-----------------------------------------------------------------------------

module TSL.TLSF
  ( toTLSF
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable
  ( stName
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.Logic
  ( Formula(..)
  , SignalTerm(..)
  , tlsfFormula
  , encodedPredicates
  , encodedUpdates
  , exactlyOne
  )

import Data.Set
  ( elems
  , fromList
  , union
  )

import Data.List
  ( groupBy
  )

import Data.Function
  ( on
  )

-----------------------------------------------------------------------------

-- | Creates the LTL under-approximation in TLSF for a given TSL
-- specification.

toTLSF
  :: String -> Specification -> String

toTLSF name Specification{..} = unlines
  [ "INFO {"
  , "  TITLE:       \"Converted TSL Specification: " ++ name ++ "\""
  , "  DESCRIPTION: \"TSL specification, which has been converted to TLSF.\""
  , "  SEMANTICS:   Mealy"
  , "  TARGET:      Mealy"
  , "}"
  , "MAIN {"
  , if null inputs
    then ""
    else unlines
      [ "  INPUTS {"
      , concatMap ((++ ";\n") . ("    " ++)) inputs ++  "  }"
      ]
  , if null outputs
    then ""
    else unlines
      [ "  OUTPUTS {"
      , concatMap ((++ ";\n") . ("    " ++)) outputs ++ "  }"
      ]
  , "  GUARANTEE {"
  , "    " ++ toTLSF (And [Globally mutual, formula])  ++ ";"
  , "  }"
  , "}"
  ]

  where
    toTLSF = tlsfFormula (stName symboltable)

    inputs = map (toTLSF . Check) $ encodedPredicates formula
    outputs = map (toTLSF . uncurry Update) updates

    updates =
      -- collect updates from the formula
      elems $ union (fromList $ encodedUpdates formula) $
      -- and add self updates
      fromList $ map ((\x -> (x, Signal x)) . fst) $ encodedUpdates formula

    mutual =
      And
        $ map (exactlyOne . map (uncurry Update))
        $ groupBy ((==) `on` fst) updates

-----------------------------------------------------------------------------
