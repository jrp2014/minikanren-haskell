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

type LogicOp a = Int -> Substitution a -> (Int, [Substitution a])

run :: LogicOp a -> [Substitution a]
run fn = subs where (_, subs) = fn 0 []

fresh :: (Term a -> LogicOp a) -> LogicOp a
fresh fn c = fn (Var $ "Var" ++ show c) (c + 1)

(===) :: Eq a => Term a -> Term a -> LogicOp a
(===) p q c subs = (c, case unify subs p q of
    Nothing -> []
    Just subs' -> [subs'])

conj :: LogicOp a -> LogicOp a -> LogicOp a
conj x y c subs = (c'', resSubs)
    where
        (c', subss) = x c subs
        (c'', subss') = y c' subs
        resSubs = do
            subs' <- subss
            subs'' <- subss'
            return $ subs' ++ subs''

conde :: LogicOp a -> LogicOp a -> LogicOp a
conde x y c subs = (c'', subss ++ subss')
    where
        (c', subss) = x c subs
        (c'', subss') = y c' subs

