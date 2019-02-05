#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module SemanticsSpec (main) where

import SATPrelude
import Semantics

main :: IO ()
main = hspec $ do
    let x1 = Name "x1"
        x2 = Name "x2"
    describe "evaluate" $ do
        it "should be true" $
            evaluate (And [Not (Var x1), Var x2]) (assign [(x1, False), (x2, True)])
                `shouldBe` Just True
        it "should be false" $
            evaluate (And [Not (Var x1), Var x2]) (assign [(x1, True), (x2, True)])
                `shouldBe` Just False
