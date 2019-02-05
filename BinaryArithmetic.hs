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

module BinaryArithmeticSpec (main) where

import PrettyOps
import SATPrelude
import Semantics
import SmtLib2
import ValidityDeduce
import ValiditySearch
import Z3

mkConst :: String -> String -> State Context Expr
mkConst name type_ = do
    ctx0 <- get
    let (c, ctx1) = declareConst ctx0 name type_
    put ctx1
    pure c

main :: IO ()
main = do
    let (expr, ctx) = flip runState emptyContext $ do
        -- a
        a1 <- mkConst "a1" "bool"
        a2 <- mkConst "a2" "bool"
        a3 <- mkConst "a3" "bool"
        a4 <- mkConst "a4" "bool"
        a5 <- mkConst "a5" "bool"
        let as = [a1, a2, a3, a4, a5]

        -- b
        b1 <- mkConst "b1" "bool"
        b2 <- mkConst "b2" "bool"
        b3 <- mkConst "b3" "bool"
        b4 <- mkConst "b4" "bool"
        b5 <- mkConst "b5" "bool"
        let bs = [b1, b2, b3, b4, b5]

        -- Carry
        c0 <- mkConst "c0" "bool"
        c1 <- mkConst "c1" "bool"
        c2 <- mkConst "c2" "bool"
        c3 <- mkConst "c3" "bool"
        c4 <- mkConst "c4" "bool"
        c5 <- mkConst "c5" "bool"
        let cs = [c1, c2, c3, c4, c5]

        -- Result digits
        d1 <- mkConst "d1" "bool"
        d2 <- mkConst "d2" "bool"
        d3 <- mkConst "d3" "bool"
        d4 <- mkConst "d4" "bool"
        d5 <- mkConst "d5" "bool"
        let ds = [d1, d2, d3, d4, d5]

        let
            -- Carry rules
            carryRules = map
                            (\(ai, bi, ci, cim1) -> Equiv cim1 (Or [And [ai, bi], And [ai, ci], And [bi, ci]]))
                            (zip4 as bs cs (c0 : cs))

            -- Result digit rules
            digitRules = map
                    (\(ai, bi, ci, di) -> Equiv di (Equiv ai (Equiv bi ci)))
                    (zip4 as bs cs ds)

            -- Carry result rule
            carryInputRule = Not c5
            carryOutputRule = Not c0

        let phi = And (carryRules ++ digitRules ++ [carryInputRule, carryOutputRule])
            e = And
                    [ phi
                    , And [Not a1, a2, a3, Not a4, a5] -- a = 13
                    , And [Not b1, Not b2, b3, b4, b5] -- b = 7
                    ]
        pure e
    let script = toSmtLib2 ctx expr
    result <- checkWithZ3 script
    for_ (lines result) putStrLn

-- Result: d1 = 1, d2 = 0, d3 = 1, d4 = 0, d5 = 0, i.e. 10100 = 20
{-
sat
(model
  (define-fun a4 () bool
    false)
  (define-fun c3 () bool
    true)
  (define-fun d4 () bool
    false)
  (define-fun a2 () bool
    true)
  (define-fun c5 () bool
    false)
  (define-fun b4 () bool
    true)
  (define-fun b5 () bool
    true)
  (define-fun b2 () bool
    false)
  (define-fun d1 () bool
    true)
  (define-fun d2 () bool
    false)
  (define-fun b1 () bool
    false)
  (define-fun a5 () bool
    true)
  (define-fun c2 () bool
    true)
  (define-fun d3 () bool
    true)
  (define-fun c4 () bool
    true)
  (define-fun a1 () bool
    false)
  (define-fun d5 () bool
    false)
  (define-fun b3 () bool
    true)
  (define-fun c0 () bool
    false)
  (define-fun c1 () bool
    true)
  (define-fun a3 () bool
    true)
-}
