{-# LANGUAGE NPlusKPatterns #-}

import Debug.Trace
import Prelude hiding ((^),(!!))
import qualified Prelude as P

su [] = 0
su (x:xs) = x + su xs

pr [] = 1
pr (x:xs) = x * pr xs

double x = x + x

quadruple x = double (double x)

-- factorial n = product [1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

mylast (xs) = head (reverse xs)

myinit1 (xs) = reverse (tail (reverse xs))

myinit2 (xs) = take (length xs - 1) xs

last1 (xs) = drop (length xs - 1) xs
-- [5]

last2 (xs) = head (drop (length xs - 1) xs)
-- 5

last3 (xs) = tail (reverse xs)
-- [4,3,2,1]

last4 (xs) = reverse (head xs)
-- error

last5 (xs) = xs !! (length xs - 1)
-- 5

last6 (xs) = head (drop (length xs) xs)
-- error: empty list

last7 (xs) = head (reverse xs)
-- 5

last8 (xs) = reverse xs !! (length xs - 1)
-- 1

init1 xs = tail (reverse xs)
-- [4,3,2,1]

init2 xs = reverse (head (reverse xs))
-- error

init3 xs = reverse (tail xs)
-- [5,4,3,2]

init4 xs = take (length xs) xs
-- [1,2,3,4,5]

init5 xs = reverse (tail (reverse xs))
-- [1,2,3,4]

init6 xs = take (length xs - 1) (tail xs)
-- [2,3,4,5]

init7 xs = drop (length xs - 1) xs
-- [5]

-- input: [3,1,4,1,5,9,2,6]
qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-- [9,6,5,4,3,2,1,1]

qsort2 [] = []
qsort2 (x:xs) = reverse (qsort2 smaller ++ [x] ++ qsort2 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-- [4,6,9,5,3,1,1,2]

qsort3 [] = []
qsort3 (x:xs) = qsort3 larger ++ qsort3 smaller ++ [x]
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-- [6,9,5,4,2,1,1,3]

qsort4 [] = []
qsort4 (x:xs) = reverse (qsort4 smaller) ++ [x] ++ reverse (qsort4 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-- [2,1,1,3,5,9,6,4]

qsort5 [] = []
qsort5 (x:xs) = qsort5 larger ++ [x] ++ qsort5 smaller
  where larger = [a | a <- xs, a > x || a == x]
        smaller = [b | b <- xs, b < x]
-- [9,6,5,4,3,2,1,1]

qsort6 [] = []
qsort6 (x:xs) = qsort6 larger ++ [x] ++ qsort6 smaller
  where smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]
-- [9,6,5,4,3,2,1]

qsort7 [] = []
qsort7 (x:xs)
  = reverse
      (reverse (qsort7 smaller) ++ [x] ++ reverse (qsort7 larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-- [9,6,5,4,3,2,1,1]

qsort8 [] = []
qsort8 xs = x : qsort8 larger ++ qsort8 smaller
  where x = maximum xs
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x]
-- infinite loop?

add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

-- halve1 xs = (take n xs, drop n xs)
--   where n  = length xs / 2

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

halve4 xs = splitAt (length xs `div` 2)

halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

halve6 xs = splitAt (div (length xs) 2) xs

-- halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- safetail5 xs = tail xs
-- safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_ : xs) = xs

safetail8
  = \ xs ->
      case xs of
           [] -> []
           (_ : xs) -> xs

e1 :: [[Int]]
e1 = [[1,2],[3,4]]

e2 :: Num t => [[[t]]]
e2 = [[[1, 2, 3]], [[3, 4, 5]]]

e3 x = x * 2

e4 (x, y) = x

e5 (x, y, z) = z

e6 x y = x * y

e7 (x, y) = (y, x)

e8 x y = (y, x)

e9 [x, y] = (x, True)

e10 (x, y) = [x, y]

e11 = ('\a', False)

e12 = [('a', 1)]

e13 :: Int -> Int -> Int
e13 x y = x + y * y

e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])

e15 :: [a] -> [b] -> (a, b)
e15 xs ys = (head xs, head ys)

-- sum100a = sum[[x * x] | x <- [1 .. 100]]

sum100b = sum [x ^ 2| x <- [1 .. 100]]

sum100c = sum [const 2 x | x <- [1 .. 100]]

sum100d = foldl (+) (1) [x ^ 2 | x <- [1 .. 100]]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions2 :: (Eq a) => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0 .. n])
  where n = length xs - 1

scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

xs11 = 1 : [x + 1 | x <- xs11]

riffle1 xs ys = concat [[x,y] | x <- xs, y <- ys]

riffle2 xs ys = concat [[x,y] | (x, y) <- xs `zip` ys]

-- riffle3 xs ys = [x, y | (x, y) <- xs `zip` ys]

riffle4 xs ys = [x : [y] | x <- xs, y <- ys]

divides :: Int -> Int -> Bool
divides a b = a `mod` b == 0

divisors :: Int -> [Int] 
divisors x = [d | d <- [1 .. x], x `divides` d]

second xs = head (tail xs)

swap (x, y) = (y, x)

pair x y = (x, y)

dbl x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)

zzzf xs = take 3 (reverse xs)

nabs n | n >= 0 = -n
       | otherwise = n

bucket n | n <  -100 = 1
         | n <  -25  = 2
         | n <= 25   = 3
         | n <= 100  = 4
         | otherwise = 5

factorial :: Int -> Int
factorial 0 = traceShow "factorial of 0 == 1" 1
factorial (n+1) = traceShow ("factorial of " ++ show (n+1) ++ " == " ++ show z) z
  where z = (n+1) * factorial n

prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * prod ns

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) | x <= y = x : y : ys
             | otherwise = y : ins x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)

zp :: [a] -> [b] -> [(a,b)]
zp [] _ = []
zp _ [] = []
zp (x:xs) (y:ys) = (x,y) : zp xs ys

drp :: Int -> [a] -> [a]
drp 0 xs = xs
drp (n+1) [] = []
drp (n+1) (_:xs) = drp n xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib (n+2) = fib n + fib (n+1)

ev :: Int -> Bool
ev 0 = True
ev (n+1) = od n

od :: Int -> Bool
od 0 = False
od (n+1) = ev n

(^) :: Int -> Int -> Int
p ^ 0 = 1
p ^ (n+1) = p * p ^ n

an :: [Bool] -> Bool
an [] = True
an (b:bs) = b && an bs

cc :: [[a]] -> [a]
cc [] = []
cc (xs:xss) = xs ++ cc xss

rep :: Int -> a -> [a]
rep 0 _ = []
rep (n+1) x = x : rep n x

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! (n+1) = xs !! n

el :: Eq a => a -> [a] -> Bool
el _ [] = False
el y (x:xs) = y == x || el y xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | y < x = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
--msort xs = merge (msort (fst (halve xs))) (msort (snd (halve xs)))
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs


zzz :: ((Bool,Char))
zzz = (True, 'c')

inc :: [Int] -> [Int]
inc = map (+1)

sqr :: [Int] -> [Int]
sqr = map (^2)

