#!/usr/bin/env stack
-- stack --resolver=lts-12.6 script

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Semantics
    ( Expr(..)
    , Interpretation
    , Name(..)
    , Value(..)
    , assign
    , emptyInterpretation
    , evaluate
    , exprAnd
    , extend
    , lookup
    , main
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (lookup)

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
    deriving Show

exprNot :: Value -> Value
exprNot ValueTrue = ValueFalse
exprNot ValueFalse = ValueTrue

exprAnd :: Value -> Value -> Value
exprAnd ValueTrue ValueTrue = ValueTrue
exprAnd _ _ = ValueFalse

exprOr :: Value -> Value -> Value
exprOr ValueTrue _ = ValueTrue
exprOr _ ValueTrue = ValueTrue
exprOr _ _ = ValueFalse

exprEquals :: Value -> Value -> Value
exprEquals ValueTrue ValueTrue = ValueTrue
exprEquals ValueFalse ValueFalse = ValueTrue
exprEquals _ _ = ValueFalse

-- | Return true if interpretation satisfies expression, false if interpretation
-- does not satisfy expression.
evaluate :: Expr -> Interpretation -> Maybe Value
evaluate ETrue _ = Just $ ValueTrue
evaluate EFalse _ = Just $ ValueFalse
evaluate (Var name) i = lookup name i
evaluate (Not f) i = exprNot <$> evaluate f i
evaluate (And f1 f2) i = exprAnd <$> evaluate f1 i <*> evaluate f2 i
evaluate (Or f1 f2) i = exprOr <$> evaluate f1 i <*> evaluate f2 i
evaluate (Implies f1 f2) i = exprOr <$> (exprNot <$> evaluate f1 i) <*> evaluate f2 i
evaluate (Equiv f1 f2) i = exprEquals <$> evaluate f1 i <*> evaluate f2 i

main :: IO ()
main = do
    let i0 = assign [(Name "x0", ValueFalse), (Name "x1", ValueFalse)]
    --print i0
    --print $ lookup (Name "x0") i0
    let i1 = extend (Name "x2") ValueTrue i0
    --print i1
    --print $ evaluate ExprTrue i1
    --print $ evaluate (ExprVar (Name "x0")) i1
    --print $ evaluate (ExprNot (ExprVar (Name "x0"))) i1
    putStrLn "main"
    print $ evaluate (Implies (Var (Name "x0")) (Var (Name "x1"))) i1

    example1
    example2

example1 :: IO ()
example1 = do
    let x1 = Name "x1"
        x2 = Name "x2"
        false = ValueFalse
        true = ValueTrue
    putStrLn "example1"
    print $ evaluate (And (Not (Var x1)) (Var x2)) (assign [(x1, false), (x2, true)])

example2 :: IO ()
example2 = do
    let x1 = Name "x1"
        x2 = Name "x2"
        false = ValueFalse
        true = ValueTrue
    putStrLn "example2"
    print $ evaluate (And (Not (Var x1)) (Var x2)) (assign [(x1, true), (x2, true)])
