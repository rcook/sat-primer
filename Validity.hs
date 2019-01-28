#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
    --package containers
    --package transformers
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Validity (main) where

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.List (nub)
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
main = do
    let x1 = Var (Name "x1")
        x2 = Var (Name "x2")

    {-
    let f1 = Implies (And x1 x2) (Or x1 (Not x2))
    print $ searchValid f1

    let f2 = Implies (Or x1 (Not x2)) (And x1 x2)
    print $ searchValid f2

    let f3 = Implies (And x1 (Implies x1 x2)) x2
    print $ searchValid f3

    let f4 = Implies (Or x1 (Not x2)) (And x1 x2)
    print $ searchValid f4
    -}

    print $
        deduceValid (Implies (And x1 (Implies x1 x2)) x2)
        == Right (Deduction {contradictions = [[ISatisfies (Var (Name "x2")),ISatisfies (Var (Name "x1")),IDoesNotSatisfy (Var (Name "x2"))],[IDoesNotSatisfy (Var (Name "x1")),ISatisfies (Var (Name "x1")),IDoesNotSatisfy (Var (Name "x2"))]], models = []})
    print $
        deduceValid (Implies (Or x1 (Not x2)) (And x1 x2))
        == Left (Deduction {contradictions = [[IDoesNotSatisfy (Var (Name "x1")),ISatisfies (Var (Name "x1"))]], models = [[IDoesNotSatisfy (Var (Name "x2")),IDoesNotSatisfy (Var (Name "x2"))],[IDoesNotSatisfy (Var (Name "x1")),IDoesNotSatisfy (Var (Name "x2"))],[IDoesNotSatisfy (Var (Name "x2")),ISatisfies (Var (Name "x1"))]]})

-- TBD: Is there a more elegant way to do this?
splitFacts :: (a -> Maybe b) -> [a] -> ([a], Maybe b, [a])
splitFacts p xs = go [] xs []
    where
        go gs (x : xs) hs =
            case p x of
                result@(Just _) -> (gs, result, xs ++ hs)
                _ -> go (gs ++ [x]) xs hs
        go gs _ hs = (gs, Nothing, hs)

data Fact =
    ISatisfies Expr
    | IDoesNotSatisfy Expr
    deriving (Eq, Show)

contra :: Fact -> Fact
contra (ISatisfies f) = IDoesNotSatisfy f
contra (IDoesNotSatisfy f) = ISatisfies f

data Conclusion =
    OneFact Fact
    | TwoFacts Fact Fact
    | Branch Fact Fact
    deriving Show

--traceConclusion :: String -> A -> Maybe A
--traceConclusion ctx res = trace (printf "rule %s: %s" ctx (show res)) (Just res)
traceConclusion :: String -> Conclusion -> Maybe Conclusion
traceConclusion _ = Just . id

matchFact :: Fact -> Maybe Conclusion
matchFact (ISatisfies (Not f)) = traceConclusion "1" $
    OneFact (IDoesNotSatisfy f)
matchFact (IDoesNotSatisfy (Not f)) = traceConclusion "2" $
    OneFact (ISatisfies f)
matchFact (ISatisfies (And f1 f2)) = traceConclusion "3" $
    TwoFacts (ISatisfies f1) (ISatisfies f2)
matchFact (IDoesNotSatisfy (And f1 f2)) = traceConclusion "4" $
    Branch (IDoesNotSatisfy f1) (IDoesNotSatisfy f2)
matchFact (ISatisfies (Or f1 f2)) = traceConclusion "5" $
    Branch (ISatisfies f1) (ISatisfies f2)
matchFact (IDoesNotSatisfy (Or f1 f2)) = traceConclusion "6" $
    TwoFacts (IDoesNotSatisfy f1) (IDoesNotSatisfy f2)
matchFact (ISatisfies (Implies f1 f2)) = traceConclusion "7" $
    Branch (IDoesNotSatisfy f1) (ISatisfies f2)
matchFact (IDoesNotSatisfy (Implies f1 f2)) = traceConclusion "8" $
    TwoFacts (ISatisfies f1) (IDoesNotSatisfy f2)
matchFact (ISatisfies (Equiv f1 f2)) = traceConclusion "9" $
    Branch (ISatisfies (And f1 f2)) (IDoesNotSatisfy (Or f1 f2))
matchFact (IDoesNotSatisfy (Equiv f1 f2)) = traceConclusion "10" $
    Branch (ISatisfies (And f1 (Not f2))) (ISatisfies (And (Not f1) f2))
matchFact _ = Nothing

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
    let facts = [IDoesNotSatisfy f]
    in case runState (allClosed facts) (Deduction [] []) of
        (False, result) -> Left result
        (True, result) -> Right result
    where
        allClosed :: [Fact] -> State Deduction Bool
        allClosed facts =
            case splitFacts matchFact facts of
                (gs, Just (OneFact f), hs) ->
                    allClosed (f : gs ++ hs)
                (gs, Just (TwoFacts f1 f2), hs) ->
                    allClosed (f1 : f2 : gs ++ hs)
                (gs, Just (Branch f1 f2), hs) -> do
                    r0 <- allClosed (f1 : gs ++ hs)
                    r1 <- allClosed (f2 : gs ++ hs)
                    pure $ r0 && r1
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
