{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValiditySearch
    ( searchSat
    , searchValid
    ) where

import SATPrelude
import Semantics

variables :: Expr -> [Name]
variables expr = nub (go expr)
    where
        go :: Expr -> [Name]
        go (Lit _) = []
        go (Var name) = [name]
        go (Not f) = go f
        go (And fs) = concatMap go fs
        go (Or fs) = concatMap go fs
        go (Implies f1 f2) = go f1 ++ go f2
        go (Equiv f1 f2) = go f1 ++ go f2

searchSat :: Expr -> Maybe Bool
searchSat expr = not <$> searchValid (Not expr)

-- Truth table method
searchValid :: Expr -> Maybe Bool
searchValid expr = allSat (variables expr) emptyInterpretation
    where
        allSat :: [Name] -> Interpretation -> Maybe Bool
        allSat (v : vs) i = (&&) <$> allSat vs (extend v False i) <*> allSat vs (extend v True i)
        allSat [] i = evaluate expr i
