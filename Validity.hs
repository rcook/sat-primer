#!/usr/bin/env stack
-- stack --resolver=lts-12.6 script --package containers

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Validity (main) where

import Data.List (nub)
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


{-
data Foo = IDoesNotSatisfy Expr

deduceValid :: Expr -> Maybe Value
deduceValid f =
    let facts = [IDoesNotSatisfy f]
    in allClosed facts
    where
        --allClosed :: _ -> Maybe Value
        allClosed facts
            | 
-}
{-
(define (deduce-valid? f)
  (let all-closed? ([facts `((I ⊭ ,f))])    ; assumption
    (match facts
      [`(,gs ... (I ⊨ (¬ ,f1)) ,hs ...)     ; (1)
       (all-closed? `((I ⊭ ,f1) ,@gs ,@hs))]
      [`(,gs ... (I ⊭ (¬ ,f1)) ,hs ...)     ; (2)
       (all-closed? `((I ⊨ ,f1) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (∧ ,fs ...)) ,hs ...) ; (3)
       (all-closed? `(,@(for/list ([fi fs]) `(I ⊨ ,fi)) ,@gs ,@hs))]
      [`(,gs ... (I ⊭ (∧ ,fs ...)) ,hs ...) ; (4)
       (for/and ([fi fs])
         (all-closed? `((I ⊭ ,fi) ,@gs ,@hs)))]
      [`(,gs ... (I ⊨ (∨ ,fs ...)) ,hs ...) ; (5)
       (for/and ([fi fs])
         (all-closed? `((I ⊨ ,fi) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (∨ ,fs ...)) ,hs ...) ; (6)
       (all-closed? `(,@(for/list ([fi fs]) `(I ⊭ ,fi)) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (→ ,f1 ,f2)) ,hs ...) ; (7)
       (and (all-closed? `((I ⊭ ,f1) ,@gs ,@hs))
            (all-closed? `((I ⊨ ,f2) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (→ ,f1 ,f2)) ,hs ...) ; (8)
       (all-closed? `((I ⊨ ,f1) (I ⊭ ,f2) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (↔ ,f1 ,f2)) ,hs ...) ; (9)
       (and (all-closed? `((I ⊨ (∧ ,f1 ,f2)) ,@gs ,@hs))
            (all-closed? `((I ⊭ (∨ ,f1 ,f2)) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (↔ ,f1 ,f2)) ,hs ...) ; (10)
       (and (all-closed? `((I ⊨ (∧ ,f1 (¬ ,f2))) ,@gs ,@hs))
            (all-closed? `((I ⊨ (∧ (¬ ,f1) ,f2)) ,@gs ,@hs)))]
      [(or `(,_ ... (I ⊨ ,fi) ,_ ... (I ⊭ ,fi) ,_ ...)
           `(,_ ... (I ⊭ ,fi) ,_ ... (I ⊨ ,fi) ,_ ...))
       (printf "Contradiction on ~a: ~a\n" fi facts)
       #t]
      [_
       (printf "Open branch: ~a\n" facts)
       #f])))
-}

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

    --print $ blah (Implies (And x1 (Implies x1 x2)) x2)
    print $ blah (Implies (Or x1 (Not x2)) (And x1 x2))

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

data A =
    OneFact Fact
    | TwoFacts Fact Fact
    | Branch Fact Fact
    deriving Show

-- Rule 1
rule (ISatisfies (Not f)) =
    let res = Just (OneFact (IDoesNotSatisfy f))
    in trace ("rule1: " ++ show res) res
-- Rule 2
rule (IDoesNotSatisfy (Not f)) =
    let res = Just (OneFact (ISatisfies f))
    in trace ("rule2: " ++ show res) res
-- Rule 3
rule (ISatisfies (And f1 f2)) =
    let res = Just (TwoFacts (ISatisfies f1) (ISatisfies f2))
    in trace ("rule3: " ++ show res) res
-- Rule 4
rule (IDoesNotSatisfy (And f1 f2)) =
    let res = Just
                (Branch
                    (IDoesNotSatisfy f1)
                    (IDoesNotSatisfy f2))
    in trace ("rule4: " ++ show res) res
-- Rule 5
rule (ISatisfies (Or f1 f2)) =
    let res = Just
                (Branch
                    (ISatisfies f1)
                    (ISatisfies f2))
    in trace ("rule5: " ++ show res) res
-- Rule 6
rule (IDoesNotSatisfy (Or f1 f2)) =
    let res = Just (TwoFacts (IDoesNotSatisfy f1) (IDoesNotSatisfy f2))
    in trace ("rule6: " ++ show res) res
-- Rule 7
rule (ISatisfies (Implies f1 f2)) =
    let res = Just
                (Branch
                    (IDoesNotSatisfy f1)
                    (ISatisfies f2))
    in trace ("rule7: " ++ show res) res
-- Rule 8
rule (IDoesNotSatisfy (Implies f1 f2)) =
    let res = Just (TwoFacts (ISatisfies f1) (IDoesNotSatisfy f2))
    in trace ("rule8: " ++ show res) res
-- Rule 9
rule (ISatisfies (Equiv f1 f2)) =
    let res = Just
                (Branch
                    (ISatisfies (And f1 f2))
                    (IDoesNotSatisfy (Or f1 f2)))
    in trace ("rule9: " ++ show res) res
-- Rule 10
rule (IDoesNotSatisfy (Equiv f1 f2)) =
    let res = Just
                (Branch
                    (ISatisfies (And f1 (Not f2)))
                    (ISatisfies (And (Not f1) f2)))
    in trace ("rule10: " ++ show res) res
-- Does not match any rule
rule r = trace ("could not match rule: " ++ show r) Nothing

hasContradiction :: Fact -> [Fact] -> Bool
hasContradiction = elem . contra

isContradictory :: [Fact] -> Bool
isContradictory fs = any ((flip hasContradiction) fs) fs

--blah :: Expr -> Maybe Value
blah f =
    let facts = [IDoesNotSatisfy f]
    in allClosed facts
    where
        allClosed facts =
            case splitFacts rule facts of
                (gs, Just (OneFact f), hs) ->
                    allClosed (f : gs ++ hs)
                (gs, Just (TwoFacts f1 f2), hs) ->
                    allClosed (f1 : f2 : gs ++ hs)
                (gs, Just (Branch f1 f2), hs) ->
                    allClosed (f1 : gs ++ hs)
                        && allClosed (f2 : gs ++ hs)
                _ ->
                    if isContradictory facts
                        then error "CONTRADICTIONS"
                        else error (show facts)
