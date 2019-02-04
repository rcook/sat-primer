#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValiditySpec (main) where

import PrettyOps
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
            searchValid (Implies (x1 ∧ x2) (x1 ∨ (¬) x2))
                `shouldBe` Just True
            searchValid (Implies (x1 ∨ (¬) x2) (x1 ∧ x2))
                `shouldBe` Just False
            searchValid (Implies (x1 ∧ (Implies x1 x2)) x2)
                `shouldBe` Just True
    describe "deduceValid" $ do
        it "proves formula" $
            deduceValid (Implies (x1 ∧ (Implies x1 x2)) x2)
                `shouldBe` Right (Result
                    { contradictions =
                        [ [Satisfies x2, Satisfies x1, Falsifies x2]
                        , [Falsifies x1, Satisfies x1, Falsifies x2]
                        ]
                    , models = []
                    })
        it "disproves formula" $
            deduceValid (Implies (x1 ∨ (¬) x2) (x1 ∧ x2))
                `shouldBe` Left (Result
                    { contradictions =
                        [ [Falsifies x1, Satisfies x1]
                        ]
                    , models =
                        [ [Falsifies x2, Falsifies x2]
                        , [Falsifies x1, Falsifies x2]
                        , [Falsifies x2, Satisfies x1]
                        ]
                    })
