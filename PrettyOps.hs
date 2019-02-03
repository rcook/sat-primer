-- Inspired by http://hackage.haskell.org/package/base-unicode-symbols-0.2.3/docs/src/Data.Bool.Unicode.html#%2227
module PrettyOps
    ( (∧)
    , (∨)
    , (¬)
    ) where

import Semantics

(¬) :: Expr -> Expr
(¬) = Not

(∧) :: Expr -> Expr -> Expr
(∧) = And
infixr 3 ∧

(∨) :: Expr -> Expr -> Expr
(∨) = Or
infixr 2 ∨
