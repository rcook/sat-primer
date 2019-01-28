#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Validity (main) where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.List (nub)
import Test.Hspec
import Text.Printf (printf)

import Debug.Trace

import SATPrelude
import Semantics hiding (main)

variables :: Expr -> [Name]
variables f = nub (go f)
    where
        go ETrue = []
        go EFalse = []
        go (Var name) = [name]
        go (Not f) = go f
        go (And f1 f2) = go f1 ++ go f2
        go (Or f1 f2) = go f1 ++ go f2
        go (Implies f1 f2) = go f1 ++ go f2
        go (Equiv f1 f2) = go f1 ++ go f2

searchValid :: Expr -> Maybe Value
searchValid f =
    let vars = variables f
        i = emptyInterpretation
    in allSat vars i
    where
        allSat :: [Name] -> Interpretation -> Maybe Value
        allSat (v : vs) i = and <$> allSat vs (extend v ValueFalse i) <*> allSat vs (extend v ValueTrue i)
        allSat [] i = evaluate f i

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
                        [ [ISatisfies (Var (Name "x2")), ISatisfies (Var (Name "x1")), IFalsifies (Var (Name "x2"))]
                        , [IFalsifies (Var (Name "x1")), ISatisfies (Var (Name "x1")), IFalsifies (Var (Name "x2"))]
                        ]
                    , models = []
                    })
        it "disproves formula" $
            deduceValid (Implies (Or x1 (Not x2)) (And x1 x2))
                `shouldBe` Left (Deduction
                    { contradictions =
                        [ [IFalsifies (Var (Name "x1")), ISatisfies (Var (Name "x1"))]
                        ]
                    , models =
                        [ [IFalsifies (Var (Name "x2")), IFalsifies (Var (Name "x2"))]
                        , [IFalsifies (Var (Name "x1")), IFalsifies (Var (Name "x2"))]
                        , [IFalsifies (Var (Name "x2")), ISatisfies (Var (Name "x1"))]
                        ]
                    })

data Fact =
    ISatisfies Expr
    | IFalsifies Expr
    deriving (Eq, Show)

contra :: Fact -> Fact
contra (ISatisfies f) = IFalsifies f
contra (IFalsifies f) = ISatisfies f

data Conclusion =
    FactList [Fact]
    | Branch [Fact]
    deriving Show

hasContradiction :: Fact -> [Fact] -> Bool
hasContradiction = elem . contra

findContradictions :: [Fact] -> [Fact]
findContradictions fs =
    foldl'
        (\cs f -> if hasContradiction f fs then f : cs else cs)
        []
        fs

data Deduction = Deduction
    { contradictions :: [[Fact]]
    , models :: [[Fact]]
    } deriving (Eq, Show)

deduceValid :: Expr -> Either Deduction Deduction
deduceValid f =
    let facts = [IFalsifies f]
    in case runState (allClosed facts) (Deduction [] []) of
        (False, result) -> Left result
        (True, result) -> Right result
    where
        allClosed :: [Fact] -> State Deduction Bool
        allClosed facts =
            case matchRule facts of
                Just xyz -> xyz
                _ ->
                    case findContradictions facts of
                        (c : _) -> do
                            d <- get
                            put $ d { contradictions = facts : contradictions d }
                            pure True
                        [] -> do
                            d <- get
                            put $ d { models = facts : models d }
                            pure False

        branch fs xs = all (== True) <$> sequence (map (allClosed . (: xs)) fs)

        matchRule xs = go [] xs []
            where
                go gs (x : xs) hs =
                    case rule x (gs ++ xs ++ hs) of
                        result@(Just _) -> result
                        _ -> go (gs ++ [x]) xs hs
                go gs _ hs = Nothing
                rule (ISatisfies (Not f)) = Just . allClosed . (IFalsifies f :)
                rule (IFalsifies (Not f)) = Just . allClosed . (ISatisfies f :)
                rule (ISatisfies (And f1 f2)) = Just . allClosed . ([ISatisfies f1, ISatisfies f2] ++)
                rule (IFalsifies (And f1 f2)) = Just . branch [IFalsifies f1, IFalsifies f2]
                rule (ISatisfies (Or f1 f2)) = Just . branch [ISatisfies f1, ISatisfies f2]
                rule (IFalsifies (Or f1 f2)) = Just . allClosed . ([IFalsifies f1, IFalsifies f2] ++)
                rule (ISatisfies (Implies f1 f2)) = Just . branch [IFalsifies f1, ISatisfies f2]
                rule (IFalsifies (Implies f1 f2)) = Just . allClosed . ([ISatisfies f1, IFalsifies f2] ++)
                rule (ISatisfies (Equiv f1 f2)) = Just . branch [ISatisfies (And f1 f2), IFalsifies (Or f1 f2)]
                rule (IFalsifies (Equiv f1 f2)) = Just . branch [ISatisfies (And f1 (Not f2)), ISatisfies (And (Not f1) f2)]
                rule _ = const Nothing
