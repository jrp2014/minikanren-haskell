module MiniKanren where

import Control.Monad (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.State

type Logic a b = StateT (Substitution a, Int) [] b  -- bind of [] is conjungtion
type LogicVar = String
data Term a
    = Var LogicVar
    | Data a
    | List [Term a]
    deriving (Show, Eq, Read)
type Substitution a = [(LogicVar, Term a)] -- TODO: Map?

run :: Logic a b -> [Substitution a]
run program = do
    (_, (s, _)) <- runStateT program ([], 0)
    return s

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

(===) :: Eq a => Term a -> Term a -> Logic a ()
t === r = do
    (s, _) <- get
    case unify s t r of
        Nothing -> lift [] -- fail
        Just res -> do
            (_, c) <- get
            put (res, c)

fresh :: Logic a (Term a)
fresh = do
    (s, c) <- get
    put (s, c + 1)
    return $ Var $ "var" ++ show c
