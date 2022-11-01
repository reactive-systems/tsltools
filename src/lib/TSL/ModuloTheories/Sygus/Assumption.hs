-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Assumption
-- Description :  Generate TSL Assumptions from SyGuS results
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Assumption
  ( sygus2TslAssumption
  ) where

-------------------------------------------------------------------------------

import Data.List (intersperse, transpose)

import Data.Text(pack, unpack, replace)

import Text.Regex.PCRE.Heavy (scan, re)

import TSL.ModuloTheories.Sygus.Common (Dto(..)
                                       , Temporal (..)
                                       , Expansion (..)
                                       , Term (..)
                                       , targetPostfix
                                       )

import TSL.ModuloTheories.Theories (TheorySymbol, symbol2Tsl)

import TSL.ModuloTheories.Predicates (pred2Tsl)

import Debug.Trace (trace)

-------------------------------------------------------------------------------

unquoteShow :: String -> String
unquoteShow = filter (/= '\"')

removePostfix :: String -> String
removePostfix = unpack . replace postfix "" . pack
  where postfix = pack targetPostfix

binOp2Tsl :: String -> String
binOp2Tsl "="   = "eq"
binOp2Tsl "<"   = "lt"
binOp2Tsl ">"   = "gt"
binOp2Tsl "<="  = "lte"
binOp2Tsl ">="  = "gte"
binOp2Tsl "+"   = "add"
binOp2Tsl "-"   = "sub"
binOp2Tsl "*"   = "mult"
binOp2Tsl "/"   = "div"
binOp2Tsl other = trace ("Got irreducible binop>" ++ other ++ "<")other

numeric2Tsl :: String -> String
numeric2Tsl value =
  case scan [re|(([+-]?[0-9]*\.[0-9]+))|] value of
    [(_,[real])] -> "real" ++ real ++ "()"
    _            ->
      case scan [re|([0-9]+)|] value of
        [(_,[int])] -> "int" ++ int ++ "()"
        []          -> value
        _           -> error $ "Unexpected value! >> " ++ value

data Update a = Update {sink :: a, source :: DataSource a} deriving (Show)

data DataSource a =
      TslValue a
    | TslFunction a [DataSource a]
    -- deriving (Show)

instance (Show a) => Show (DataSource a) where
  show = \case
    TslValue literal   -> numeric2Tsl $ postprocess $ show literal
    TslFunction f args -> unwords [ binOp2Tsl $ postprocess $ show f
                                  , unwords $ map (postprocess . show) args
                                  ]
    where postprocess = removePostfix . unquoteShow

update2Tsl :: (Show a) => Update a -> String
update2Tsl update =
  unquoteShow $ "[" ++ show (sink update)  ++ " <- " ++ show (source update) ++ "]"

updates2Tsl :: (Show a) => [[Update a]] -> String
updates2Tsl updates =
  -- trace ("SHOWING UPDATES> " ++ show updates ++ " <END") $
  unwords $ intersperse tslAnd $ zipWith depth2Assumption [0..] $ reverse updates
  where
    tslAnd = "&&"

    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth 'X'
            anded = unwords $ intersperse tslAnd $ map update2Tsl depthUpdates

term2DataSource :: Term a -> DataSource a
term2DataSource = \case
  Value value          -> TslValue value
  Expression expansion -> TslValue $ nonterminal expansion
  Function f args      -> TslFunction f $ map term2DataSource args

term2Updates :: Term a -> [[Update a]]
term2Updates = \case
  Value _              -> []
  Expression expansion -> calculateUpdateChain expansion
  Function _ args      -> map concat $ transpose $ map term2Updates args

calculateUpdateChain :: Expansion a -> [[Update a]]
calculateUpdateChain (Expansion dst term) =
  case term of 
    Value      value        -> [[Update dst (TslValue value)]]

    Expression expansion    -> [update]:updates
        where updates = calculateUpdateChain expansion
              update  = Update dst $ TslValue $ nonterminal expansion

    function@(Function _ _) -> [update]:updates
        where updates = term2Updates function
              update  = Update dst $ term2DataSource function

sygus2TslAssumption :: Temporal -> Dto -> Term String -> String
sygus2TslAssumption temporal (Dto _ pre post) term = unwords
  [ "G"
  , "(" -- GLOBALLY
  , "(" -- PRE + UPDATES
  , pred2Tsl pre
  , "&&"
  , updateTerm
  , ")" -- PRE + UPDATES
  , "->"
  , show temporal
  , "(" ++ pred2Tsl post ++ ")"
  , ")" -- GLOBALLY
  , ";"
  ]
  where
    updateChain = updates2Tsl $ term2Updates term
    updateTerm  = if (temporal == Eventually)
                     then updateChain ++ " W " ++ pred2Tsl post
                     else updateChain
