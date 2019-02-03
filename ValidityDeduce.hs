{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module ValidityDeduce
    ( Deduction(..)
    , Fact(..)
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

data Deduction2 = DeductionAnd [Fact] | DeductionFork [Fact]

-- | Proof rules mapping premises to deductions
proofRule ::
    Fact                -- ^ premise
    -> Maybe Deduction2 -- ^ deduction
proofRule (ISatisfies (Not f)) = Just $ DeductionAnd [IFalsifies f]
proofRule (IFalsifies (Not f)) = Just  $ DeductionAnd [ISatisfies f]
proofRule (ISatisfies (And f1 f2)) = Just $ DeductionAnd [ISatisfies f1, ISatisfies f2]
proofRule (IFalsifies (And f1 f2)) = Just $ DeductionFork [IFalsifies f1, IFalsifies f2]
proofRule (ISatisfies (Or f1 f2)) = Just $ DeductionFork [ISatisfies f1, ISatisfies f2]
proofRule (IFalsifies (Or f1 f2)) = Just $ DeductionAnd [IFalsifies f1, IFalsifies f2]
proofRule (ISatisfies (Implies f1 f2)) = Just $ DeductionFork [IFalsifies f1, ISatisfies f2]
proofRule (IFalsifies (Implies f1 f2)) = Just $ DeductionAnd [ISatisfies f1, IFalsifies f2]
proofRule (ISatisfies (Equiv f1 f2)) = Just $ DeductionFork [ISatisfies (And f1 f2), IFalsifies (Or f1 f2)]
proofRule (IFalsifies (Equiv f1 f2)) = Just $ DeductionFork [ISatisfies (And f1 (Not f2)), ISatisfies (And (Not f1) f2)]
proofRule _ = Nothing

allClosed :: [Fact] -> State Deduction Bool
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
