#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module SemanticsDemo (main) where

import SATPrelude
import Semantics

main :: IO ()
main = do
    let i0 = assign [(Name "x0", ValueFalse), (Name "x1", ValueFalse)]
    --print i0
    --print $ lookup (Name "x0") i0
    let i1 = extend (Name "x2") ValueTrue i0
    --print i1
    --print $ evaluate ExprTrue i1
    --print $ evaluate (ExprVar (Name "x0")) i1
    --print $ evaluate (ExprNot (ExprVar (Name "x0"))) i1
    putStrLn "main"
    print $ evaluate (Implies (Var (Name "x0")) (Var (Name "x1"))) i1

    example1
    example2

example1 :: IO ()
example1 = do
    let x1 = Name "x1"
        x2 = Name "x2"
        false = ValueFalse
        true = ValueTrue
    putStrLn "example1"
    print $ evaluate (And (Not (Var x1)) (Var x2)) (assign [(x1, false), (x2, true)])

example2 :: IO ()
example2 = do
    let x1 = Name "x1"
        x2 = Name "x2"
        true = ValueTrue
    putStrLn "example2"
    print $ evaluate (And (Not (Var x1)) (Var x2)) (assign [(x1, true), (x2, true)])
