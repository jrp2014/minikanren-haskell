module MiniKanren
  ( LogicVar
  , Term(..)
  , Environment(..)
  , Substitution
  , Goal
  , QueryProgram
  , runAll
  , run
  , runStep
  , fresh
  , freshs
  , (===)
  , (=/=)
  , conj
  , conde
  , condeDepthFirst
  , (/\)
  , (\/)
  , emptyEnv
  , anyo
  ) where

import Control.Monad (foldM)
import Data.List (transpose)
import System.IO (hFlush, stdout)

-- | Logic variables are repesented as strings
type LogicVar = String

-- | a term is either a logic variable, some haskell data or a list of terms
data Term a
  = Var LogicVar
  | Atom a
  | List [Term a]
  deriving (Eq, Read)

instance Show a => Show (Term a) where
  show (Var v) = '_' : v
  show (Atom a) = show a
  show (List l) = show l

-- | The state of the minikanren interpreter
data Environment a = Environment
  { counter :: Int -- ^ a count to create new intermediate variables
  , substitution :: Substitution a -- ^ a substitution for evalues to be known equal
  , disEqStore :: [Substitution a] -- ^ a list of substitutions for things to be known inequal
                                     --   (these are seperated because there might be not true at the same time
  } deriving (Show)

emptyEnv :: Environment a
emptyEnv = Environment {counter = 0, substitution = [], disEqStore = [[]]}

-- | A Substitution is a mapping from logic variables to other terms
type Substitution a = [(LogicVar, Term a)]

-- | A goal takes a state and returns every possible state produced by this operation
--   e.g.: Input state: x -> 1
--         Operation: y = 2 \/ y = 3
--         List of possible output states: x -> 1, y -> 2; x -> 1, y -> 3
type Goal a = Environment a -> [Environment a]

-- | a query program is a goal that takes an input term
type QueryProgram a = Term a -> Goal a

-- | returns all values for a variable "q" in a query program and their inequalities
runAll :: QueryProgram a -> [(Term a, [Term a])]
runAll program =
  map
    (\ps ->
       ( reify (Var "q") (substitution ps)
       , map (flip walk $ Var "q") $ disEqStore ps)) $
  program (Var "q") emptyEnv

-- | returns the first n values for a variable "q" in a query program and there inequalities
run :: Int -> QueryProgram a -> [(Term a, [Term a])]
run solutions program = take solutions $ runAll program

-- | runs a query program interactivly (link in a prolog intepreter)
--   just hit enter (insert blank lines) to get more solutions
--   type anything else to exit
runStep :: Show a => QueryProgram a -> IO Bool
runStep program = loop (runAll program)
  where
    loop :: Show a => [a] -> IO Bool
    loop [] = return False
    loop (subs:subss) = do
      putStr (show subs ++ " ")
      hFlush stdout
      userAction <- getLine
      case userAction of
        "" -> loop subss
        _ -> return True

-- | creates a new logic variable and runs it inside a Goal
fresh :: QueryProgram a -> Goal a
fresh fn env = fn (Var $ show $ counter env) (env {counter = counter env + 1})

-- | creates n new logic variables and runs them inside a Goal
freshs :: Int -> ([Term a] -> Goal a) -> Goal a
freshs n fn env =
  fn
    (map (Var . show) [counter env .. counter env + n - 1])
    (env {counter = counter env + n})

-- | assert the equality of two terms, changes the local state (substitution)
infix 4 ===

(===) :: Eq a => Term a -> Term a -> Goal a
(===) p q ps =
  case unify (substitution ps) p q of
    Just subs
      | checkAssertions subs ps -> [ps {substitution = subs}]
    _ -> []

-- | try to get a substitution witch makes the given terms equal
unify :: Eq a => Substitution a -> Term a -> Term a -> Maybe (Substitution a)
unify subs t u =
  case (walk subs t, walk subs u) of
    (Var v, Var w)
      | v == w -> return subs
    (Var v, term) -> return $ (v, term) : subs
    (term, Var v) -> return $ (v, term) : subs
    (List vs, List us)
      | length vs == length us ->
        foldM (\subs' (v, w) -> unify subs' v w) subs $ zip vs us
    (Atom x, Atom y)
      | x == y -> return subs
    _ -> Nothing

-- | check if all inequalities are satisfied
checkAssertions :: Eq a => Substitution a -> Environment a -> Bool
checkAssertions subs ps = all exclude (disEqStore ps)
  where
    exclude noSubs =
      all (\(var, _) -> walk noSubs (Var var) /= walk subs (Var var)) noSubs

-- | assert the inequality of two terms, changes the local state (disEqStore)
infix 4 =/=

(=/=) :: Eq a => Term a -> Term a -> Goal a
(=/=) p q ps =
  case unify (substitution ps) p q of
    Nothing -> return ps
    Just subs
                    -- we fail here if they are already equal
                    -- so we dont need checkAssertions here
      | length subs == length (substitution ps) -> []
      | otherwise ->
        return $
        ps
          { disEqStore =
              take (length subs - length (substitution ps)) subs : disEqStore ps
          }

-- | helper to create a goal bunch of conjugated goals
conj :: [Goal a] -> Goal a
conj = foldr (/\) return

-- | conjunction: combine two goals such that both are satisfied
infixl 3 /\

(/\) :: Goal a -> Goal a -> Goal a
(/\) x y ps = concatMap y $ x ps
-- /\ = flip (<=<)

-- | uses depth first search for disjunction
condeDepthFirst :: [Goal a] -> Goal a
condeDepthFirst = foldr condeDepthFirstSearch' (const [])
  where
    condeDepthFirstSearch' --forall a t.
     ::
         (t -> [a]) -> (t -> [a]) -> t -> [a]
    condeDepthFirstSearch' x y ps = x ps ++ y ps

-- | helper to create a goal bunch of disjunct goals
conde :: [Goal a] -> Goal a
conde = foldr (\/) (const [])

-- | helper to to create a goal with a conjunction of goals
programme :: [Goal a] -> Goal a
programme = foldr (/\) return

-- | disjunction: combines two goals such that one or both of them are satisfied
infixl 2 \/

(\/) :: Goal a -> Goal a -> Goal a
--(\/) a b ps = (concat . transpose) [a ps,  b ps] -- fairer?
(\/) a b ps = a ps ++ b ps -- depth first?

anyo :: Goal a -> Goal a
anyo g = conde $ repeat g

--anyo g = conde [g, anyo g]
-- | apply a substitution on a term, and replacing all variables by there equal terms
walk :: Substitution a -> Term a -> Term a
walk subs var@(Var v) = maybe var (walk subs) (lookup v subs)
walk _ term = term

-- | every variable in the term is either substituted using the substitution or by a default substitution
reify :: Term a -> Substitution a -> Term a
reify v s = walkStar v' (reifyS v' [])
  where
    v' = walkStar v s

-- | walk a term with a substitution until walk dosen't produce new nested structures
walkStar :: Term a -> Substitution a -> Term a
walkStar v s =
  case v' of
    List terms -> List $ map (`walkStar` s) terms
    _ -> v'
  where
    v' = walk s v

-- | adds default a substitution for every variable in the term
reifyS :: Term a -> Substitution a -> Substitution a
reifyS v s =
  case walk s v of
    Var name -> (name, Var $ "_" ++ show (length s)) : s
    List terms -> foldl (flip reifyS) s terms
    Atom _ -> s
