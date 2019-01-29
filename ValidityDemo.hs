#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValidityDemo (main) where

import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    )

import SATPrelude
import Semantics
import ValidityDeduce
import ValiditySearch

main :: IO ()
main = hspec $ do
    let x1 = Var (Name "x1")
        x2 = Var (Name "x2")
    describe "searchValid" $
        it "searches successfully" $ do
            searchValid (Implies (And x1 x2) (Or x1 (Not x2)))
                `shouldBe` Just ValueTrue
            searchValid (Implies (Or x1 (Not x2)) (And x1 x2))
                `shouldBe` Just ValueFalse
            searchValid (Implies (And x1 (Implies x1 x2)) x2)
                `shouldBe` Just ValueTrue
    describe "deduceValid" $ do
        it "proves formula" $
            deduceValid (Implies (And x1 (Implies x1 x2)) x2)
                `shouldBe` Right (Deduction
                    { contradictions =
                        [ [ISatisfies x2, ISatisfies x1, IFalsifies x2]
                        , [IFalsifies x1, ISatisfies x1, IFalsifies x2]
                        ]
                    , models = []
                    })
        it "disproves formula" $
            deduceValid (Implies (Or x1 (Not x2)) (And x1 x2))
                `shouldBe` Left (Deduction
                    { contradictions =
                        [ [IFalsifies x1, ISatisfies x1]
                        ]
                    , models =
                        [ [IFalsifies x2, IFalsifies x2]
                        , [IFalsifies x1, IFalsifies x2]
                        , [IFalsifies x2, ISatisfies x1]
                        ]
                    })
