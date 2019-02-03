#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module SATDemo (main) where

import PrettyOps
import SATPrelude
import Semantics
import ValiditySearch

var :: String -> Expr
var = Var . Name

main :: IO ()
main = hspec $ do
    describe "examples from https://www.coursera.org/learn/automated-reasoning-sat week 1" $ do
        it "finds example 1 satisfiable" $ do
            let p = var "p"
                q = var "q"
                expr = (p ∨ q) ∧ ((¬) p ∨ (¬) q) ∧ (p ∨ (¬) q)
            expr `shouldBe` And (Or (Var (Name "p")) (Var (Name "q"))) (And (Or (Not (Var (Name "p"))) (Not (Var (Name "q")))) (Or (Var (Name "p")) (Not (Var (Name "q")))))
            searchSat expr `shouldBe` Just True
        it "finds example 2 unsatisfiable" $ do
            let p = var "p"
                q = var "q"
                expr = (p ∨ q) ∧ ((¬) p ∨ (¬) q) ∧ ((¬) p ∨ q) ∧ (p ∨ (¬) q)
            expr `shouldBe` And (Or (Var (Name "p")) (Var (Name "q"))) (And (Or (Not (Var (Name "p"))) (Not (Var (Name "q")))) (And (Or (Not (Var (Name "p"))) (Var (Name "q"))) (Or (Var (Name "p")) (Not (Var (Name "q"))))))
            searchSat expr `shouldBe` Just False
