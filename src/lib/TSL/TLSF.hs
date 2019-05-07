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
  , tlsfPredicates
  , tlsfUpdates
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
  , "    " ++ tlsfFormula (And [Globally mutual, fml])  ++ ";"
  , "  }"
  , "}"
  ]

  where
    fml = fmap (stName symboltable) formula

    updates =
      elems $ union (fromList $ tlsfUpdates fml) $
      fromList $ map ((\x -> (x, Signal x)) . fst) $
      tlsfUpdates fml

    inputs = map (tlsfFormula . Check) $ tlsfPredicates fml

    outputs = map (tlsfFormula . uncurry Update) updates

    mutual =
      And
        $ map (exactlyOne . map (uncurry Update))
        $ groupBy ((==) `on` fst) updates

-----------------------------------------------------------------------------
