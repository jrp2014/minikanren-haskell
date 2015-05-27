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

unify :: Eq a => Substitution a -> Term a -> Term a -> Maybe (Substitution a)
unify subs t u = case (walk subs t, walk subs u) of
    (Var v, Var w) | v == w -> return subs
    (Var v, term) -> return $ (v, term) : subs
    (term, Var v) -> return $ (v, term) : subs
    (List vs, List us) | length vs == length us ->
        foldM (\subs' (v, w) -> unify subs' v w) subs $ zip vs us
    (Data x, Data y) | x == y -> return subs
    _ -> Nothing

fresh :: (LogicVar -> Int -> Substitution a -> [Substitution a]) ->
          Int -> Substitution a -> [Substitution a]
fresh = undefined

(===) :: Term a -> Term a ->
         Int -> Substitution a -> [Substitution a]
(===) = undefined

conj :: (Int -> Substitution a -> [Substitution a]) ->
        (Int -> Substitution a -> [Substitution a]) ->
        Int -> Substitution a -> [Substitution a]
conj = undefined

conde :: (Int -> Substitution a -> [Substitution a]) ->
         (Int -> Substitution a -> [Substitution a]) ->
         Int -> Substitution a -> [Substitution a]
conde = undefined

