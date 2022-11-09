-------------------------------------------------------------------------------
-- |
-- Module      :  TSL.ModuloTheories.Sygus.Update
-- Description :  ``Update`` data structure
-- Maintainer  :  Wonhyuk Choi

-------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

-------------------------------------------------------------------------------
module TSL.ModuloTheories.Sygus.Update
  ( DataSource (..)
  , Update (..)
  , term2Updates
  ) where

-------------------------------------------------------------------------------

import Data.List (transpose)

import Data.Maybe (catMaybes)

import Text.Regex.PCRE.Heavy (scan, re)

import TSL.ModuloTheories.Sygus.Common (Expansion (..)
                                       , Term (..)
                                       )

-------------------------------------------------------------------------------

unquoteShow :: String -> String
unquoteShow = filter (/= '\"')

showOperator :: String -> String
showOperator "="   = "eq"
showOperator "<"   = "lt"
showOperator ">"   = "gt"
showOperator "<="  = "lte"
showOperator ">="  = "gte"
showOperator "+"   = "add"
showOperator "-"   = "sub"
showOperator "*"   = "mult"
showOperator "/"   = "div"
showOperator other = other

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
  deriving (Eq)

data Update a = Update {sink :: a, source :: DataSource a}
  deriving (Eq)

instance (Show a) => Show (DataSource a) where
  show = \case
    TslValue literal   -> numeric2Tsl $ unquoteShow $ show literal
    TslFunction f args -> unwords [ showOperator $ unquoteShow $ show f
                                  , unwords $ map (unquoteShow . show) args
                                  ]

instance (Show a) => Show (Update a) where
  show update = bracket $ unwords [ unquoteShow $ show $ sink update
                                  , updateSymbol
                                  , show $ source update
                                  ]
    where bracket str  = "[" ++ str ++ "]"
          updateSymbol = "<-"

instance Functor DataSource where
  fmap f = \case
    TslValue a         -> TslValue $ f a
    TslFunction a args -> TslFunction (f a) $ map (fmap f) args

instance Functor Update where
  fmap f update = Update (f (sink update)) (fmap f (source update))

removeSelfUpdates :: (Eq a) => [[Update a]] -> [[Update a]]
removeSelfUpdates = catMaybes . (map removeSelves)
  where removeSelves xs = case catMaybes ((map removeSelfUpdate) xs) of
                         [] -> Nothing 
                         ys -> Just ys

        removeSelfUpdate update = case (source update) of
            TslFunction _ _   -> Just $ update
            TslValue sinkTerm -> if (sink update) == sinkTerm
                                    then Nothing
                                    else Just $ update

term2DataSource :: Term a -> DataSource a
term2DataSource = \case
  Value value          -> TslValue value
  Expression expansion -> TslValue $ nonterminal expansion
  Function f args      -> TslFunction f $ map term2DataSource args

term2Updates :: (Eq a) => Term a -> [[Update a]]
term2Updates = \case
  Value _              -> []
  Expression expansion -> removeSelfUpdates $ calculateUpdateChain expansion
  Function _ args      -> map concat $ transpose $ map term2Updates args

calculateUpdateChain :: (Eq a) => Expansion a -> [[Update a]]
calculateUpdateChain (Expansion dst term) =
  case term of 
    Value      value        -> [[Update dst (TslValue value)]]

    Expression expansion    -> [update]:updates
        where updates = calculateUpdateChain expansion
              update  = Update dst $ TslValue $ nonterminal expansion

    function@(Function _ _) -> [update]:updates
        where updates = term2Updates function
              update  = Update dst $ term2DataSource function
