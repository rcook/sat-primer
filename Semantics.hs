{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Semantics
    ( Expr(..)
    , Interpretation
    , Name(..)
    , Value(..)
    , and
    , assign
    , emptyInterpretation
    , evaluate
    , extend
    , lookup
    , not
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SATPrelude

newtype Name = Name String deriving (Eq, Ord, Show)
data Value = ValueTrue | ValueFalse deriving (Eq, Show)
type Interpretation = Map Name Value

emptyInterpretation :: Interpretation
emptyInterpretation = Map.empty

assign :: [(Name, Value)] -> Interpretation
assign = Map.fromList

lookup :: Name -> Interpretation -> Maybe Value
lookup = Map.lookup

extend :: Name -> Value -> Interpretation -> Interpretation
extend = Map.insert

data Expr =
    ETrue
    | EFalse
    | Var Name
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Implies Expr Expr
    | Equiv Expr Expr
    deriving (Eq, Show)

not :: Value -> Value
not ValueTrue = ValueFalse
not ValueFalse = ValueTrue

and :: Value -> Value -> Value
and ValueTrue ValueTrue = ValueTrue
and _ _ = ValueFalse

or :: Value -> Value -> Value
or ValueTrue _ = ValueTrue
or _ ValueTrue = ValueTrue
or _ _ = ValueFalse

equals :: Value -> Value -> Value
equals ValueTrue ValueTrue = ValueTrue
equals ValueFalse ValueFalse = ValueTrue
equals _ _ = ValueFalse

-- | Return true if interpretation satisfies expression, false if interpretation
-- does not satisfy expression.
evaluate :: Expr -> Interpretation -> Maybe Value
evaluate ETrue _ = Just $ ValueTrue
evaluate EFalse _ = Just $ ValueFalse
evaluate (Var name) i = lookup name i
evaluate (Not f) i = not <$> evaluate f i
evaluate (And f1 f2) i = and <$> evaluate f1 i <*> evaluate f2 i
evaluate (Or f1 f2) i = or <$> evaluate f1 i <*> evaluate f2 i
evaluate (Implies f1 f2) i = or <$> (not <$> evaluate f1 i) <*> evaluate f2 i
evaluate (Equiv f1 f2) i = equals <$> evaluate f1 i <*> evaluate f2 i
