#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package process
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

-- Examples from https://www.coursera.org/learn/automated-reasoning-sat

module CourseraSpec (main) where

import PrettyOps
import SATPrelude
import Semantics
import SmtLib2
import ValiditySearch
import Z3

var :: String -> Expr
var = Var . Name

main :: IO ()
main = hspec $ do
    describe "week 1" $ do
        it "finds example 1 satisfiable" $ do
            let p = var "p"
                q = var "q"
                expr = (p ∨ q) ∧ ((¬) p ∨ (¬) q) ∧ (p ∨ (¬) q)
            expr `shouldBe` And [Or [Var (Name "p"), Var (Name "q")], And [Or [Not (Var (Name "p")), Not (Var (Name "q"))], Or [Var (Name "p"), Not (Var (Name "q"))]]]
            searchSat expr `shouldBe` Just True
        it "finds example 2 unsatisfiable" $ do
            let p = var "p"
                q = var "q"
                expr = (p ∨ q) ∧ ((¬) p ∨ (¬) q) ∧ ((¬) p ∨ q) ∧ (p ∨ (¬) q)
            expr `shouldBe` And [Or [Var (Name "p"), Var (Name "q")], And [Or [Not (Var (Name "p")), Not (Var (Name "q"))], And [Or [Not (Var (Name "p")), Var (Name "q")], Or [Var (Name "p"), Not (Var (Name "q"))]]]]
            searchSat expr `shouldBe` Just False
        it "finds example 3 unsatisfiable" $ do
            let p = var "p"
                q = var "q"
                r = var "r"
                expr = (¬) ((p ∧ q) `Implies` (p ∨ r))
            expr `shouldBe` Not (Implies (And [Var (Name "p"), Var (Name "q")]) (Or [Var (Name "p"), Var (Name "r")]))
            searchSat expr `shouldBe` Just False
    describe "toSmtLib2" $
        it "works" $ do
            let ctx0 = emptyContext
                (a, ctx1) = declareConst ctx0 "a" "bool"
                (b, ctx2) = declareConst ctx1 "b" "bool"
                (c, ctx3) = declareConst ctx2 "c" "bool"
                (d, ctx4) = declareConst ctx3 "d" "bool"
                expr = (a `Equiv` (d ∧ b)) ∧ (c `Implies` b) ∧ (¬) (a ∨ b ∨ (¬) d) ∧ (((¬) a ∧ c) ∨ d)
                script = toSmtLib2 ctx4 expr
            result <- checkWithZ3 script
            "sat" `isPrefixOf` result `shouldBe` True
