#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package containers
        --package hspec
        --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wall #-}

module Validity (main) where

import Control.Monad.Trans.State.Strict
    ( State
    , get
    , put
    , runState
    )
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    )

import SATPrelude
import Semantics hiding (main)

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

variables :: Expr -> [Name]
variables expr = nub (go expr)
    where
        go :: Expr -> [Name]
        go ETrue = []
        go EFalse = []
        go (Var name) = [name]
        go (Not e) = go e
        go (And e1 e2) = go e1 ++ go e2
        go (Or e1 e2) = go e1 ++ go e2
        go (Implies e1 e2) = go e1 ++ go e2
        go (Equiv e1 e2) = go e1 ++ go e2

searchValid :: Expr -> Maybe Value
searchValid expr = allSat (variables expr) emptyInterpretation
    where
        allSat :: [Name] -> Interpretation -> Maybe Value
        allSat (v : vs) i = and <$> allSat vs (extend v ValueFalse i) <*> allSat vs (extend v ValueTrue i)
        allSat [] i = evaluate expr i

data Fact =
    ISatisfies Expr
    | IFalsifies Expr
    deriving (Eq, Show)

contra :: Fact -> Fact
contra (ISatisfies f) = IFalsifies f
contra (IFalsifies f) = ISatisfies f

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
deduceValid expr = case runState (allClosed [IFalsifies expr]) (Deduction [] []) of
    (False, result) -> Left result
    (True, result) -> Right result
    where
        allClosed :: [Fact] -> State Deduction Bool
        allClosed facts =
            case matchRule facts of
                Just p -> p
                _ ->
                    case findContradictions facts of
                        [] -> do
                            d <- get
                            put $ d { models = facts : models d }
                            pure False
                        _ -> do
                            d <- get
                            put $ d { contradictions = facts : contradictions d }
                            pure True

        branch :: [Fact] -> [Fact] -> State Deduction Bool
        branch fs xs = all (== True) <$> sequence (map (allClosed . (: xs)) fs)

        matchRule :: [Fact] -> Maybe (State Deduction Bool)
        matchRule xs = go [] xs []
            where
                go :: [Fact] -> [Fact] -> [Fact] -> Maybe (State Deduction Bool)
                go gs (y : ys) hs =
                    case rule y (gs ++ ys ++ hs) of
                        result@(Just _) -> result
                        _ -> go (gs ++ [y]) ys hs
                go _ _ _ = Nothing

                rule :: Fact -> [Fact] -> Maybe (State Deduction Bool)
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
