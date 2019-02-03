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

{-
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

data Conclusion = CAnd [Fact] | CBranch [Fact]

prule :: Fact -> Maybe Conclusion
prule (ISatisfies (Not f)) = Just $ CAnd [IFalsifies f]
prule (IFalsifies (Not f)) = Just  $ CAnd [ISatisfies f]
prule (ISatisfies (And f1 f2)) = Just $ CAnd [ISatisfies f1, ISatisfies f2]
prule (IFalsifies (And f1 f2)) = Just $ CBranch [IFalsifies f1, IFalsifies f2]
prule (ISatisfies (Or f1 f2)) = Just $ CBranch [ISatisfies f1, ISatisfies f2]
prule (IFalsifies (Or f1 f2)) = Just $ CAnd [IFalsifies f1, IFalsifies f2]
prule (ISatisfies (Implies f1 f2)) = Just $ CBranch [IFalsifies f1, ISatisfies f2]
prule (IFalsifies (Implies f1 f2)) = Just $ CAnd [ISatisfies f1, IFalsifies f2]
prule (ISatisfies (Equiv f1 f2)) = Just $ CBranch [ISatisfies (And f1 f2), IFalsifies (Or f1 f2)]
prule (IFalsifies (Equiv f1 f2)) = Just $ CBranch [ISatisfies (And f1 (Not f2)), ISatisfies (And (Not f1) f2)]
prule _ = Nothing

allClosed' :: [Fact] -> Bool
allClosed' fs =
    case breakOnJust prule fs of
        (gs, Just (CAnd fs'), hs) -> allClosed' (fs' ++ gs ++ hs)
        (gs, Just (CBranch fs'), hs) -> all (== True) (map (\f -> allClosed' (f : gs ++ hs)) fs')
        (gs, Nothing, hs) ->
            case findContradictions fs of
                [] -> False
                _ -> True
-}
