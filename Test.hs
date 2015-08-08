module Test where

import MiniKanren

appendo xs ys res =
    conde
        [xs === List [] /\ ys === res,
         freshs 3 $ \[car, cdr, rec] ->
            conj [xs === List [car, cdr],
                  res === List [car, rec],
                  appendo cdr ys rec]]

toList :: Term a -> [Term a]
toList (List [car, cdr]) = car : toList cdr
toList (List []) = []
toList x = [x]

fromList :: [a] -> Term a
fromList xs = foldr (\x acc -> List [Data x, acc]) (List []) xs

test = runAll $ \q -> freshs 3 $ \[a,b,c] ->
    conj
        [a === fromList [1,2,3],
         b === fromList [4,5],
         appendo a b c,
         q === List [a,b,c]]

testBack1 = runAll $ \a -> freshs 2 $ \[b,c] ->
    conj
        [c === fromList [1,2,3,4,5],
         b === fromList [4,5],
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

testRembero = runAll $ \q -> rembero (Data 2) (fromList [1, 2, 3, 2, 4]) q



