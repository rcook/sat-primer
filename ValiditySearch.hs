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
        go (Lit _) = []
        go (Var name) = [name]
        go (Not e) = go e
        go (And e1 e2) = go e1 ++ go e2
        go (Or e1 e2) = go e1 ++ go e2
        go (Implies e1 e2) = go e1 ++ go e2
        go (Equiv e1 e2) = go e1 ++ go e2

searchValid :: Expr -> Maybe Bool
searchValid expr = allSat (variables expr) emptyInterpretation
    where
        allSat :: [Name] -> Interpretation -> Maybe Bool
        allSat (v : vs) i = (&&) <$> allSat vs (extend v False i) <*> allSat vs (extend v True i)
        allSat [] i = evaluate expr i
