module Test where

import MiniKanren

-- appendo
appendo :: Eq a => Term a -> Term a -> Term a -> LogicOp a
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
nsDF :: Int -> Term Int -> LogicOp Int
nsDF n x = condeDepthFirst [x === Data n, nsDF n x]

testNumbersDF :: [(Term Int, [Term Int])]
testNumbersDF = run 10 $ \x -> condeDepthFirst [nsDF 5 x, nsDF 6 x]

ns :: Int -> Term Int -> LogicOp Int
ns n x = conde [x === Data n, ns n x]

testNumbers :: [(Term Int, [Term Int])]
testNumbers = run 10 $ \x -> conde [ns 5 x, ns 6 x]


-- rembero
rembero :: Eq a => Term a -> Term a -> Term a -> LogicOp a
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
testRembero = runAll $ \q -> rembero (Data 2) (fromList [1, 2, 3, 2, 4]) q

-- util
toList :: Term a -> [Term a]
toList (List [car, cdr]) = car : toList cdr
toList (List []) = []
toList x = [x]

fromList :: [a] -> Term a
fromList = foldr (\x acc -> List [Data x, acc]) (List [])


