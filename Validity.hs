#!/usr/bin/env stack
-- stack --resolver=lts-12.6 script --package containers

{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Validity (main) where

import Data.List (nub)
import Semantics hiding (main)

variables :: Expr -> [Name]
variables f = nub (go f)
    where
        go ETrue = []
        go EFalse = []
        go (Var name) = [name]
        go (Not f) = go f
        go (And f1 f2) = go f1 ++ go f2
        go (Or f1 f2) = go f1 ++ go f2
        go (Implies f1 f2) = go f1 ++ go f2
        go (Equiv f1 f2) = go f1 ++ go f2

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
    let x1 = Var (Name "x1")
        x2 = Var (Name "x2")

    let f1 = Implies (And x1 x2) (Or x1 (Not x2))
    print $ searchValid f1

    let f2 = Implies (Or x1 (Not x2)) (And x1 x2)
    print $ searchValid f2
