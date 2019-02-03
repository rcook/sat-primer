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
    , isPrefixOf
    , it
    , map
    , not
    , nub
    , print
    , printf
    , pure
    , putStrLn
    , sequence
    , shouldBe
    ) where

import Data.Foldable (foldl')
import Data.List (isPrefixOf, nub)
import Prelude
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    )
import Text.Printf (printf)
