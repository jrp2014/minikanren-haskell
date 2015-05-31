module MiniKanren where

import Control.Monad (foldM)
import System.IO (hFlush, stdout)

type LogicVar = String
data Term a
    = Var LogicVar
    | Data a
    | List [Term a]
    deriving (Show, Eq, Read)
type Substitution a = [(LogicVar, Term a)] -- TODO: Map?
type LogicOp a = Int -> Substitution a -> [(Int, Substitution a)]
type QueryProgram a = Term a -> LogicOp a

runAll :: QueryProgram a -> [Term a]
runAll program = map (reify (Var "q") . snd) $ program (Var "q") 0 []

run :: Int -> QueryProgram a -> [Term a]
run solutions program = take solutions $ runAll program

runStep :: Show a => QueryProgram a -> IO Bool
runStep program = loop (runAll program)
    where
        loop [] = return False
        loop (subs:subss) = do
            putStr (show subs ++ " "); hFlush stdout
            userAction <- getLine
            case userAction of
                "" -> loop subss
                _ -> return True

fresh :: (Term a -> LogicOp a) -> LogicOp a
fresh fn c = fn (Var $ show c) (c + 1)

freshs :: Int -> ([Term a] -> LogicOp a) -> LogicOp a
freshs n fn c = fn (map (Var . show) [c..c+n-1]) (c+n)

(===) :: Eq a => Term a -> Term a -> LogicOp a
(===) p q c subs = case unify subs p q of
    Nothing -> []
    Just subs' -> [(c, subs')]

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

reify :: Term a -> Substitution a -> Term a
reify v s = walkStar v' (reifyS v' [])
    where v' = walkStar v s

walkStar :: Term a -> Substitution a -> Term a
walkStar v s = case v' of
    List terms -> List $ map (`walkStar` s) terms
    _ -> v'
    where v' = walk s v

reifyS :: Term a -> Substitution a -> Substitution a
reifyS v s = case walk s v of
    Var name -> (name, Var $ "_" ++ show (length s)) : s
    List terms -> foldl (flip reifyS) s terms
    Data _ -> s

conj :: [LogicOp a] -> LogicOp a
conj = foldr conj1 (\c subs -> [(c, subs)])
    where conj1 x y c subs = concatMap (uncurry y) $ x c subs

condeDepthFirst :: [LogicOp a] -> LogicOp a
condeDepthFirst = foldr condeDepthFirst1 (\_ _ -> [])
    where condeDepthFirst1 x y c subs = x c subs ++ y c subs

conde :: [LogicOp a] -> LogicOp a
conde = foldr conde1 (\_ _ -> [])
    where
        conde1 x y c subs = together (x c subs) (y c subs)
        together [] xs = xs
        together (x:xs) ys = x : together ys xs

