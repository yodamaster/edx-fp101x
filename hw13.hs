{-# LANGUAGE NPlusKPatterns #-}

module Homework13 where

fff :: [a] -> a -> [a]
fff = (\xs x -> x : xs)

aaa :: [a]
aaa = []

bbb :: [Char]
bbb = ['a'..'e'] ++ ['1'..'5'] ++ ['f'..'h']

ex0 = foldl fff aaa bbb

foldl'1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a
ex1 = foldl'1 fff aaa bbb

foldl'2 f a bs = foldr (\a b -> f b a) a bs
ex2 = foldl'2 fff aaa bbb

foldl'3 f = flip $ foldr (\a b g -> b (f g a)) id
ex3 = foldl'3 fff aaa bbb

foldl'4 = foldr . flip
ex4 = foldl'4 fff aaa bbb

