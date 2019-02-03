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

import SATPrelude
import Semantics
import ValiditySearch


var :: String -> Expr
var = Var . Name

main :: IO ()
main = hspec $ do
    describe "examples from https://www.coursera.org/learn/automated-reasoning-sat week 1" $ do
        it "solves example 1" $ do
            let p = var "p"
                q = var "q"
            searchSat (((p `Or` q) `And` (Not p `Or` Not q)) `And` (p `Or` Not q))
                `shouldBe` Just True
