-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Assumption
-- Description :  Generate TSL Assumptions from SyGuS results
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Assumption
  ( sygus2TslAssumption
  ) where

-------------------------------------------------------------------------------

import Data.Maybe (catMaybes)

import Data.List (intersperse, transpose)

import Text.Regex.PCRE.Heavy (scan, re)

import TSL.ModuloTheories.Sygus.Common (Dto(..)
                                       , Temporal (..)
                                       , Expansion (..)
                                       , Term (..)
                                       )

import TSL.ModuloTheories.Theories (TheorySymbol, symbol2Tsl)

import TSL.ModuloTheories.Predicates (pred2Tsl)

-------------------------------------------------------------------------------

unquoteShow :: String -> String
unquoteShow = filter (/= '\"')

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
binOp2Tsl other = other

numeric2Tsl :: String -> String
numeric2Tsl value =
  case scan [re|(([+-]?[0-9]*\.[0-9]+))|] value of
    [(_,[real])] -> "real" ++ real ++ "()"
    _            ->
      case scan [re|([0-9]+)|] value of
        [(_,[int])] -> "int" ++ int ++ "()"
        []          -> value
        _           -> error $ "Unexpected value! >> " ++ value

data DataSource a =
      TslValue a
    | TslFunction a [DataSource a]

data Update a = Update {sink :: a, source :: DataSource a}

instance (Show a) => Show (DataSource a) where
  show = \case
    TslValue literal   -> numeric2Tsl $ unquoteShow $ show literal
    TslFunction f args -> unwords [ binOp2Tsl $ unquoteShow $ show f
                                  , unwords $ map (unquoteShow . show) args
                                  ]

instance (Show a) => Show (Update a) where
  show update = bracket $ unwords [ unquoteShow $ show $ sink update
                                  , updateSymbol
                                  , show $ source update
                                  ]
    where bracket str  = "[" ++ str ++ "]"
          updateSymbol = "<-"

removeSelfUpdate :: (Eq a) => Update a -> Maybe (Update a)
removeSelfUpdate update = case (source update) of
    TslFunction _ _   -> Just $ update
    TslValue sinkTerm -> if (sink update) == sinkTerm
                            then Nothing
                            else Just $ update

updates2Tsl :: (Eq a, Show a) => [[Update a]] -> String
updates2Tsl updates = unwords $ intersperse tslAnd depthAssumptions
  where
    tslAnd           = "&&"
    tslNext          = 'X'
    trueUpdates      = getTrueUpdates $ reverse updates
    depthAssumptions = zipWith depth2Assumption [0..] trueUpdates

    getTrueUpdates :: (Eq a) => [[Update a]] -> [[Update a]]
    getTrueUpdates = catMaybes . (map removeSelves)
      where removeSelves xs = case catMaybes ((map removeSelfUpdate) xs) of
                             [] -> Nothing 
                             ys -> Just ys

    depth2Assumption :: (Show a) => Int -> [Update a] -> String
    depth2Assumption depth depthUpdates = nexts ++ "(" ++ anded ++ ")"
      where nexts = replicate depth tslNext
            anded = unwords $ intersperse tslAnd $ map show depthUpdates

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
sygus2TslAssumption temporal (Dto _ pre post) term = 
  if null updateChain
     then "// [x <- x] type assumptions not yet supported."
     else unwords [ "G"
                  , "(" -- GLOBALLY
                  , "(" -- PRE + UPDATES
                  , pred2Tsl pre
                  , updateTerm
                  , ")" -- PRE + UPDATES
                  , "->"
                  , show temporal
                  , "(" ++ pred2Tsl post ++ ")"
                  , ")" -- GLOBALLY
                  , ";"
                  ]
  where
    tslAnd      = "&&"
    weakUntil   = " W "
    updateChain = updates2Tsl $ term2Updates term
    updateTerm  = let condition = if temporal == Eventually
                                     then weakUntil ++ pred2Tsl post
                                     else ""
                   in tslAnd ++ updateChain ++ condition
