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

module TSL.TOML
  ( toTOML
  ) where

-----------------------------------------------------------------------------

import TSL.SymbolTable
  ( stName
  )

import TSL.Specification
  ( Specification(..)
  )

import TSL.Logic
  ( tslFormula
  )

-----------------------------------------------------------------------------

-- | Creates the LTL under-approximation in TOML for a given TSL
-- specification.

toTOML
  :: String -> Specification -> String

toTOML name Specification{..} = unlines $
  [ "# TITLE:       \"Converted TSL Specification: " ++ name ++ "\""
  , "# DESCRIPTION: \"TSL specification, which has been converted to TOML.\""
  , "# SEMANTICS:   Mealy"
  , "# TARGET:      Mealy"
  , "" ]
  ++ (
  if null assumptions
  then
    []
  else
    [ "[assumptions]"
    , "  initially = [" ]
    ++ (map (\f -> "    '" ++ tslFormula (stName symboltable) f ++ "',") assumptions) ++
    [ "  ]" ]
  ) ++ (
  if null guarantees
  then
    []
  else
    [ "[guarantees]"
    , "  initially = [" ]
    ++ (map (\f -> "    '" ++ tslFormula (stName symboltable) f ++ "',") guarantees) ++
    [ "  ]" ]
  )

-----------------------------------------------------------------------------
