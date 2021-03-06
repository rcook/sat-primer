module SmtLib2
    ( Context
    , declareConst
    , emptyContext
    , toSmtLib2
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SATPrelude
import Semantics

data Const = Const String String deriving Show

data Context = Context [Const] deriving Show

emptyContext :: Context
emptyContext = Context []

declareConst :: Context -> String -> String -> (Expr, Context)
declareConst (Context consts) name type_ =
    let v = Var (Name name)
    in (v, Context (Const name type_ : consts))

toSmtLib2 :: Context -> Expr -> String
toSmtLib2 (Context consts) expr =
    let decls = concatMap (\(Const name type_) -> printf "(declare-const %s %s)\n" name type_) consts
    in decls ++ printf "(assert %s)\n(check-sat)\n(get-model)\n" (assertBody expr)

assertBody :: Expr -> String
assertBody (Lit value) = printf "(%s)" (show value)
assertBody (Var (Name name)) = name
assertBody (Not f) = printf "(not %s)" (assertBody f)
assertBody (And fs) = printf "(and%s)" $ concatMap (\f -> ' ' : assertBody f) fs
assertBody (Or fs) = printf "(or%s)" $ concatMap (\f -> ' ' : assertBody f) fs
assertBody (Implies f1 f2) = printf "(implies %s %s)" (assertBody f1) (assertBody f2)
assertBody (Equiv f1 f2) = printf "(iff %s %s)" (assertBody f1) (assertBody f2)
