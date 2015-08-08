module MiniKanren (
  LogicVar, Term(..), ProgramState(..),
  Substitution, LogicOp, QueryProgram,
  runAll, run, runStep,
  fresh, freshs,
  (===), (=/=),
  conj, conde, condeDepthFirst,
  (/\), (\/)
  ) where

import Control.Monad (foldM)
import System.IO (hFlush, stdout)

type LogicVar = String
data Term a
    = Var LogicVar
    | Data a
    | List [Term a]
    deriving (Show, Eq, Read)
data ProgramState a = ProgramState
    { counter :: Int
    , substitution :: Substitution a
    , disEqStore :: [Substitution a]
    }
type Substitution a = [(LogicVar, Term a)]
type LogicOp a = ProgramState a -> [ProgramState a]
type QueryProgram a = Term a -> LogicOp a

runAll :: QueryProgram a -> [(Term a, [Term a])]
runAll program = map (\ps ->
        (reify (Var "q") (substitution ps),
         map (flip walk $ Var "q") $ disEqStore ps)) $
    program (Var "q") $ ProgramState 0 [] []

run :: Int -> QueryProgram a -> [(Term a, [Term a])]
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
fresh fn ps = fn (Var $ show $ counter ps) (ps { counter = counter ps + 1 })

freshs :: Int -> ([Term a] -> LogicOp a) -> LogicOp a
freshs n fn ps = fn (map (Var . show) [counter ps..counter ps + n-1])
    (ps { counter = counter ps + n })

infix 4 ===
(===) :: Eq a => Term a -> Term a -> LogicOp a
(===) p q ps = case unify (substitution ps) p q of
    Just subs | checkAssertions subs ps -> [ps { substitution = subs }]
    _ -> []

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

checkAssertions :: Eq a => Substitution a -> ProgramState a -> Bool
checkAssertions subs ps = all exclude (disEqStore ps)
    where exclude noSubs = all (\(var, _) ->
              walk noSubs (Var var) /= walk subs (Var var)) noSubs

infix 4 =/=
(=/=) :: Eq a => Term a -> Term a -> LogicOp a
(=/=) p q ps = case unify (substitution ps) p q of
                 Nothing -> return ps
                 Just subs
                    -- we fail here if there are allready equal
                    -- so we dont need checkAssertions here
                    | length subs == length (substitution ps) -> []
                    | otherwise -> return $ ps { disEqStore =
                            take (length subs - length (substitution ps)) subs
                            : disEqStore ps }

conj :: [LogicOp a] -> LogicOp a
conj = foldr (/\) return

infixl 3 /\
(/\) :: LogicOp a -> LogicOp a -> LogicOp a
(/\) x y ps = concatMap y $ x ps

condeDepthFirst :: [LogicOp a] -> LogicOp a
condeDepthFirst = foldr condeDepthFirst1 (const [])
    where condeDepthFirst1 x y ps = x ps ++ y ps

conde :: [LogicOp a] -> LogicOp a
conde = foldr (\/) (const [])

infixl 2 \/
(\/) :: LogicOp a -> LogicOp a -> LogicOp a
(\/) a b ps = together (a ps) (b ps)
    where
        together [] xs = xs
        together (x:xs) ys = x : together ys xs

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

