#!/usr/bin/env stack
-- stack --resolver=lts-12.6 script --package containers

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Validity (main) where

import Data.List (nub)
import Semantics hiding (main)

variables :: Expr -> [Name]
variables f = nub (go f)
    where
        go ExprTrue = []
        go ExprFalse = []
        go (ExprVar name) = [name]
        go (ExprNot f) = go f
        go (ExprAnd f1 f2) = go f1 ++ go f2
        go (ExprOr f1 f2) = go f1 ++ go f2
        go (ExprImplies f1 f2) = go f1 ++ go f2
        go (ExprEquiv f1 f2) = go f1 ++ go f2

searchValid :: Expr -> Maybe Value
searchValid f =
    let vars = variables f
        i = emptyInterpretation
    in allSat vars i
    where
        allSat :: [Name] -> Interpretation -> Maybe Value
        allSat (v : vs) i = exprAnd <$> allSat vs (extend v ValueFalse i) <*> allSat vs (extend v ValueTrue i)
        allSat [] i = evaluate f i


main :: IO ()
main = do
    let x1 = ExprVar (Name "x1")
        x2 = ExprVar (Name "x2")

    let f1 = ExprImplies (ExprAnd x1 x2) (ExprOr x1 (ExprNot x2))
    print $ searchValid f1

    let f2 = ExprImplies (ExprOr x1 (ExprNot x2)) (ExprAnd x1 x2)
    print $ searchValid f2
