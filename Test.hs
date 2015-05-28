module Test where

import MiniKanren

appendo xs ys res = do
    conde
        (conj
            (xs === List [])
            (ys === res))
        (fresh $ \car ->
            fresh $ \cdr ->
                fresh $ \rec ->
                    conj
                        (xs === List [car, cdr])
                        (conj
                            (res === List [car, rec])
                            (appendo cdr ys rec)))

toList :: Term a -> [Term a]
toList (List [car, cdr]) = car : toList cdr
toList (List []) = []
toList x = [x]

fromList :: [Term a] -> Term a
fromList (t:ts) = List [t, fromList ts]
fromList [] = List []

test = run $
    fresh $ \a ->
        fresh $ \b ->
            fresh $ \c ->
                conj
                    (a === fromList (map Data [1,2,3]))
                    (conj
                        (b === fromList (map Data [4,5]))
                        (appendo a b c))

testBack1 = run $
    fresh $ \a ->
        fresh $ \b ->
            fresh $ \c ->
                conj
                    (c === fromList (map Data [1,2,3,4,5]))
                    (conj
                        (b === fromList (map Data [4,5]))
                        (appendo a b c))
