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
a ∧ b = And [a, b]
infixr 3 ∧

(∨) :: Expr -> Expr -> Expr
a ∨ b = Or [a, b]
infixr 2 ∨
