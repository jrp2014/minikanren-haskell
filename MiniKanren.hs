module MiniKanren where

import Control.Monad (foldM)

type LogicVar = String
data Term a
    = Var LogicVar
    | Data a
    | List [Term a]
    deriving (Show, Eq, Read)
type Substitution a = [(LogicVar, Term a)] -- TODO: Map?

walk :: Substitution a -> Term a -> Term a
walk subs (Var var) = case lookup var subs of
    Nothing -> Var var
    Just term -> walk subs term
walk _    other     = other

