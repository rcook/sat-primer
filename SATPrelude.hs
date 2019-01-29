{-# LANGUAGE NoImplicitPrelude #-}

module SATPrelude
    ( ($)
    , (<$>)
    , (<*>)
    , (++)
    , (&&)
    , (||)
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
    , describe
    , elem
    , flip
    , foldl'
    , hspec
    , id
    , it
    , map
    , not
    , nub
    , print
    , pure
    , putStrLn
    , sequence
    , shouldBe
    ) where

import Data.Foldable (foldl')
import Data.List (nub)
import Prelude
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    )
