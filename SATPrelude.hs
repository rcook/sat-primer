{-# LANGUAGE NoImplicitPrelude #-}

module SATPrelude
    ( ($)
    , (<$>)
    , (<*>)
    , (++)
    , (&&)
    , (.)
    , Bool(..)
    , Either(..)
    , Eq(..)
    , IO
    , Maybe(..)
    , Ord(..)
    , Show(..)
    , String
    , all
    , any
    , const
    , elem
    , flip
    , foldl'
    , id
    , map
    , nub
    , print
    , pure
    , putStrLn
    , sequence
    ) where

import Data.Foldable (foldl')
import Data.List (nub)
import Prelude
