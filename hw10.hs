{-# LANGUAGE NPlusKPatterns #-}

module Homework10 where

import Data.List
import Data.Char
import Unsafe.Coerce

-- From lecture
subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

-- hw questions
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y : ys)
    | x == y = ys
    | otherwise = y : removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)
