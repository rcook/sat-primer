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
