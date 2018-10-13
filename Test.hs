module Test where

import MiniKanren

-- appendo
appendo :: Eq a => Term a -> Term a -> Term a -> Goal a
appendo xs ys res =
    conde
        [xs === List [] /\ ys === res,
         freshs 3 $ \[car, cdr, rec] ->
            conj [xs === List [car, cdr],
                  res === List [car, rec],
                  appendo cdr ys rec]]

testAppendo :: [(Term Int, [Term Int])]
testAppendo = runAll $ \q -> freshs 3 $ \[a,b,c] ->
    conj
        [a === fromList [1,2,3],
         b === fromList [4,5],
         appendo a b c,
         q === List [a,b,c]]

testBack1 :: [(Term Int, [Term Int])]
testBack1 = runAll $ \a -> freshs 2 $ \[b,c] ->
    conj
        [c === fromList [1,2,3,4,5],
         b === fromList [4,5],
         appendo a b c]

-- ns df search test
nsDF :: Int -> Term Int -> Goal Int
nsDF n x = condeDepthFirst [x === Atom n, nsDF n x]

testNumbersDF :: [(Term Int, [Term Int])]
testNumbersDF = run 10 $ \x -> condeDepthFirst [nsDF 5 x, nsDF 6 x]

ns :: Int -> Term Int -> Goal Int
ns n x = conde [x === Atom n, ns n x]

testNumbers :: [(Term Int, [Term Int])]
testNumbers = run 10 $ \x -> conde [ns 5 x, ns 6 x]


-- rembero
rembero :: Eq a => Term a -> Term a -> Term a -> Goal a
rembero x ls out = conde
    [conj
        [ls === List [],
         out === List []],
     freshs 2 $ \[a,d] ->
       conj
        [ls === List [a, d],
         a === x,
         d === out],
     freshs 3 $ \[a, d, res] ->
       conj
        [ls === List [a, d],
         a =/= x,
         out === List [a, res],
         rembero x d res]]

testRembero :: [(Term Int, [Term Int])]
testRembero = runAll $ \q -> rembero (Atom 2) (fromList [1, 2, 3, 2, 4]) q

-- util
toList :: Term a -> [Term a]
toList (List [car, cdr]) = car : toList cdr
toList (List []) = []
toList x = [x]

fromList :: [a] -> Term a
fromList = foldr (\x acc -> List [Atom x, acc]) (List [])

emptyEnv :: Environment a
emptyEnv = Environment {counter=0, substitution=[], disEqStore=[[]]}

aAndB :: Goal String
aAndB = 
    (fresh (\a -> a === Atom "7")) /\
    (fresh (\b -> (b === Atom "5") \/(b === Atom "6")))

-- The followmg should all give  [(3,[_q])]
test1 = runAll $ \y -> freshs 2 $ \[x, z] -> x === z /\ Atom 3 === y
test2 = runAll $ \y -> freshs 2 $ \[x, z] -> conj [x === z, Atom 3 === y]
test3 = runAll $ \q -> freshs 2 $ \[x, z] -> conj [x === z, Atom 3 === z,
                                                   q === x]
test4 = runAll $ \y -> (freshs 2 $ \[x, y] -> conj [Atom 4 === x,
                                                    x === y]) /\
                       Atom 3 === y


test6 = run 20 $ \q -> anyo (conde [ Atom 1 === q, Atom 2 === q,
                                     Atom 3 === q])
