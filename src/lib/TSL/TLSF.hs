{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  TSL.TLSF
-- Maintainer  :  Felix Klein
--
-- TLSF writer, which transforms a TSL formula into TLSF.
module TSL.TLSF
  ( toTLSF,
    tlsfToTslTerm,
  )
where

-----------------------------------------------------------------------------

import Data.Function (on)
import Data.List
  ( groupBy,
    isPrefixOf,
  )
import Data.Set (elems, toList, union)
import qualified Data.Set as S (map)
import TSL.Logic
  ( Formula (..),
    SignalTerm (..),
    checks,
    decodeInputAP,
    decodeOutputAP,
    exactlyOne,
    outputs,
    tlsfFormula,
    tslFormula,
    updates,
  )
import TSL.Specification (Specification (..), toFormula)
import TSL.SymbolTable (stName)

-----------------------------------------------------------------------------

-- | Creates the LTL under-approximation in TLSF for a given TSL
-- specification.
toTLSF ::
  String -> Specification -> String
toTLSF name Specification {..} =
  unlines
    [ "INFO {",
      "  TITLE:       \"Converted TSL Specification: " ++ name ++ "\"",
      "  DESCRIPTION: \"TSL specification, which has been converted to TLSF.\"",
      "  SEMANTICS:   Mealy",
      "  TARGET:      Mealy",
      "}",
      "MAIN {",
      if null ins
        then ""
        else
          unlines
            [ "  INPUTS {",
              concatMap ((++ ";\n") . ("    " ++)) ins ++ "  }"
            ],
      if null outs
        then ""
        else
          unlines
            [ "  OUTPUTS {",
              concatMap ((++ ";\n") . ("    " ++)) outs ++ "  }"
            ],
      "  ASSUME {",
      unlines $ map (\x -> "    " ++ toTLSF x ++ ";") assumptions,
      "  }\n",
      "  GUARANTEE {",
      unlines $ map (\x -> "    " ++ toTLSF x ++ ";") (mutual ++ [formula]),
      "  }",
      "}"
    ]
  where
    formula = toFormula assumptions guarantees

    toTLSF :: Formula Int -> String
    toTLSF =
      tlsfFormula (stName symboltable)

    ins =
      map (toTLSF . Check) $
        toList $
          checks formula

    outs =
      map (toTLSF . uncurry Update) upds

    upds =
      elems $
        union (updates formula) $
          S.map (\x -> (x, Signal x)) $
            outputs formula

    mutual =
      map (Globally . exactlyOne . map (uncurry Update)) $
        groupBy ((==) `on` fst) upds

-----------------------------------------------------------------------------

-- | Translates tlsf term back into a TSL predicate or update term
--   only works on tslf generated from a TSL spec
tlsfToTslTerm :: String -> String
tlsfToTslTerm t =
  if isPrefixOf "p0" t
    then generateTSLString Check decodeInputAP t
    else generateTSLString (uncurry Update) decodeOutputAP t

generateTSLString :: forall a b. _ -> (String -> Either a b) -> String -> String
generateTSLString tslType decoder x =
  either (const "ERR") (\t -> (tslFormula id $ tslType t)) $
    (decoder) x
