{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Semantics
    ( Expr(..)
    , Interpretation
    , Name(..)
    , assign
    , emptyInterpretation
    , evaluate
    , extend
    , lookup
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SATPrelude

newtype Name = Name String deriving (Eq, Ord, Show)
type Interpretation = Map Name Bool

emptyInterpretation :: Interpretation
emptyInterpretation = Map.empty

assign :: [(Name, Bool)] -> Interpretation
assign = Map.fromList

lookup :: Name -> Interpretation -> Maybe Bool
lookup = Map.lookup

extend :: Name -> Bool -> Interpretation -> Interpretation
extend = Map.insert

data Expr =
    Lit Bool
    | Var Name
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | Equiv Expr Expr
    deriving (Eq, Show)

-- | Return true if interpretation satisfies expression, false if interpretation
-- does not satisfy expression.
evaluate :: Expr -> Interpretation -> Maybe Bool
evaluate (Lit value) _ = Just value
evaluate (Var name) i = lookup name i
evaluate (Not f) i = not <$> evaluate f i
evaluate (And f1 f2) i = (&&) <$> evaluate f1 i <*> evaluate f2 i
evaluate (Or f1 f2) i = (||) <$> evaluate f1 i <*> evaluate f2 i
evaluate (Implies f1 f2) i = (||) <$> (not <$> evaluate f1 i) <*> evaluate f2 i
evaluate (Equiv f1 f2) i = (==) <$> evaluate f1 i <*> evaluate f2 i
