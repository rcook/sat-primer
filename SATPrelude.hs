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
    , State
    , String
    , all
    , any
    , concatMap
    , const
    , describe
    , elem
    , flip
    , foldl'
    , for_
    , get
    , hspec
    , id
    , isPrefixOf
    , it
    , lines
    , map
    , not
    , nub
    , print
    , printf
    , pure
    , put
    , putStrLn
    , runState
    , sequence
    , shouldBe
    , zip
    , zip3
    , zip4
    ) where

import Control.Monad.Trans.State.Strict
    ( State
    , get
    , put
    , runState
    )
import Data.Foldable (foldl', for_)
import Data.List (isPrefixOf, nub, zip4)
import Prelude
import Test.Hspec
    ( describe
    , hspec
    , it
    , shouldBe
    )
import Text.Printf (printf)
