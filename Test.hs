module Test where

import MiniKanren

appendo xs ys res =
    conde
        [conj [xs === List [], ys === res],
         freshs 3 $ \[car, cdr, rec] ->
            conj [xs === List [car, cdr],
                  res === List [car, rec],
                  appendo cdr ys rec]]

toList :: Term a -> [Term a]
toList (List [car, cdr]) = car : toList cdr
toList (List []) = []
toList x = [x]

fromList :: [Term a] -> Term a
fromList (t:ts) = List [t, fromList ts]
fromList [] = List []

test = runAll $ \q -> freshs 3 $ \[a,b,c] ->
    conj
        [a === fromList (map Data [1,2,3]),
         b === fromList (map Data [4,5]),
         appendo a b c,
         q === List [a,b,c]]

testBack1 = runAll $ \a -> freshs 2 $ \[b,c] ->
    conj
        [c === fromList (map Data [1,2,3,4,5]),
         b === fromList (map Data [4,5]),
         appendo a b c]

nsDF n x = condeDepthFirst [x === Data n, nsDF n x]
testNumbersDF = run 10 $ \x -> condeDepthFirst [nsDF 5 x, nsDF 6 x]

ns n x = conde [x === Data n, ns n x]
testNumbers = run 10 $ \x -> conde [ns 5 x, ns 6 x]

-- warum funktioniert das nicht: (geht in eine unendliche schleife)
-- für together?
-- together [] xs = xs
-- together xs [] = xs
-- together (x:xs) (y:ys) = x : y : together xs ys

-- bei dem falschen
-- sixes = together [6] sixes
--       = 6 : (head $ together [6] sixes) : together [6] sixes
--       = 6 : (head $ 6 : (head $ together [6] sixes) : together [6] sixes) :
--            together [6] sixes
-- bei dem richtigen:
-- sixes = together [6] sixes
--       = x : together sixes []
--       = x : x : together
