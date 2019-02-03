{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValidityDeduce
    ( Fact(..)
    , Result(..)
    , deduceValid
    ) where

import Control.Monad.Trans.State.Strict
    ( State
    , get
    , put
    , runState
    )

import SATPrelude
import Semantics

-- | Validation monad
type Validation a = State Result a

data Fact =
    Satisfies Expr
    | Falsifies Expr
    deriving (Eq, Show)

data Result = Result
    { contradictions :: [[Fact]]
    , models :: [[Fact]]
    } deriving (Eq, Show)

deduceValid :: Expr -> Either Result Result
deduceValid expr = case runState (allClosed [Falsifies expr]) (Result [] []) of
    (False, result) -> Left result
    (True, result) -> Right result

-- | 'breakOnJust', applied to a function @f@ and a list @xs@, returns a triple
-- where the first element is the prefix (possibly empty) of @xs@ of elements
-- for which the function @f@ evaluates to @Nothing@, the second element is the
-- result of applying @f@ to the first element in @xs@ for which it evaluates to
-- non-@Nothing@ and the third element is the (possibly empty) remainder of the
-- list.
breakOnJust ::
    (a -> Maybe b)          -- ^ function @f@
    -> [a]                  -- ^ list @xs@
    -> ([a], Maybe b, [a])  -- ^ result
breakOnJust _ [] = ([], Nothing, [])
breakOnJust f (x : xs) =
    case f x of
        result@(Just _) -> ([], result, xs)
        _ -> let (gs, result, hs) = breakOnJust f xs in (x : gs, result, hs)

data Deduction = DeductionAnd [Fact] | DeductionFork [Fact]

allClosed :: [Fact] -> Validation Bool
allClosed facts =
    case breakOnJust proofRule facts of
        (gs, Just (DeductionAnd fs'), hs) -> allClosed (fs' ++ gs ++ hs)
        (gs, Just (DeductionFork fs'), hs) -> all (== True) <$> sequence (map (\f -> allClosed (f : gs ++ hs)) fs')
        (_, Nothing, _) ->
            case findContradictions facts of
                [] -> do
                    d <- get
                    put $ d { models = facts : models d }
                    pure False
                _ -> do
                    d <- get
                    put $ d { contradictions = facts : contradictions d }
                    pure True

contra :: Fact -> Fact
contra (Satisfies f) = Falsifies f
contra (Falsifies f) = Satisfies f

hasContradiction :: Fact -> [Fact] -> Bool
hasContradiction = elem . contra

findContradictions :: [Fact] -> [Fact]
findContradictions fs =
    foldl'
        (\cs f -> if hasContradiction f fs then f : cs else cs)
        []
        fs

-- | Proof rules mapping premises to deductions
proofRule ::
    Fact                -- ^ premise
    -> Maybe Deduction  -- ^ deduction
proofRule (Satisfies (Not f)) = Just $ DeductionAnd [Falsifies f]
proofRule (Falsifies (Not f)) = Just  $ DeductionAnd [Satisfies f]
proofRule (Satisfies (And f1 f2)) = Just $ DeductionAnd [Satisfies f1, Satisfies f2]
proofRule (Falsifies (And f1 f2)) = Just $ DeductionFork [Falsifies f1, Falsifies f2]
proofRule (Satisfies (Or f1 f2)) = Just $ DeductionFork [Satisfies f1, Satisfies f2]
proofRule (Falsifies (Or f1 f2)) = Just $ DeductionAnd [Falsifies f1, Falsifies f2]
proofRule (Satisfies (Implies f1 f2)) = Just $ DeductionFork [Falsifies f1, Satisfies f2]
proofRule (Falsifies (Implies f1 f2)) = Just $ DeductionAnd [Satisfies f1, Falsifies f2]
proofRule (Satisfies (Equiv f1 f2)) = Just $ DeductionFork [Satisfies (And f1 f2), Falsifies (Or f1 f2)]
proofRule (Falsifies (Equiv f1 f2)) = Just $ DeductionFork [Satisfies (And f1 (Not f2)), Satisfies (And (Not f1) f2)]
proofRule _ = Nothing
