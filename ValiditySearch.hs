{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValiditySearch
    ( searchValid
    ) where

import SATPrelude
import Semantics

variables :: Expr -> [Name]
variables expr = nub (go expr)
    where
        go :: Expr -> [Name]
        go ETrue = []
        go EFalse = []
        go (Var name) = [name]
        go (Not e) = go e
        go (And e1 e2) = go e1 ++ go e2
        go (Or e1 e2) = go e1 ++ go e2
        go (Implies e1 e2) = go e1 ++ go e2
        go (Equiv e1 e2) = go e1 ++ go e2

searchValid :: Expr -> Maybe Value
searchValid expr = allSat (variables expr) emptyInterpretation
    where
        allSat :: [Name] -> Interpretation -> Maybe Value
        allSat (v : vs) i = and <$> allSat vs (extend v ValueFalse i) <*> allSat vs (extend v ValueTrue i)
        allSat [] i = evaluate expr i
