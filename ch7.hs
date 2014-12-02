module Chapter7 where

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\ x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

ex1list :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ex1list f p [] = []
ex1list f p xs = [f x | x <- xs, p x]

ex1hof :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ex1hof f p xs = map f (filter p xs)

ex1comp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
ex1comp f p = map f . filter p

al :: (a -> Bool) -> [a] -> Bool
al _ [] = True
al p (x:xs) = (p x) && al p xs
-- not quite right! al p = foldr (&&) True

ay :: (a -> Bool) -> [a] -> Bool
ay _ [] = False
ay p (x:xs) = (p x) || ay p xs

tw :: (a -> Bool) -> [a] -> [a]
tw _ [] = []
tw p (x:xs) | p x = x : tw p xs
            | otherwise = []

dw :: (a -> Bool) -> [a] -> [a]
dw _ [] = []
dw p (x:xs) | p x = dw p xs
            | otherwise = x : xs

mp :: (a -> b) -> [a] -> [b]
-- mp f xs = [f x | x <- xs]
-- mp f [] = []
-- mp f (x:xs) = f x : mp f xs
mp f = foldl (\ xs x -> xs ++ [f x]) []
-- mp f = foldr (\ x xs -> [f x] ++ xs) []

fil :: (a -> Bool) -> [a] -> [a]
-- fil p [] = []
-- fil p (x:xs) | p x = x : fil p xs
--              | otherwise = fil p xs
-- fil p = foldr (\ x xs -> if p x then [x] ++ xs else xs) []
fil p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
-- dec2int = foldl (\ x y -> y + 10 * x) 0
dec2int = foldl (\ x y -> 10 * x + y) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sse = compose [map (^2), filter even]

crry :: ((a,b) -> c) -> (a -> b -> c)
crry f = \ x y -> f (x,y)

uncrry :: (a -> b -> c) -> ((a,b) -> c)
uncrry f = \ (x,y) -> f x y

ccc :: (Int, Char) -> (Char, Int)
ccc (n,c) = (c,n)

uuu :: Int -> Char -> (Char, Int)
uuu n c = (c,n)

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

ch8 :: [Bit] -> [[Bit]]
ch8 = unfold null (take 8) (drop 8)

mmpp :: (a -> b) -> [a] -> [b]
mmpp f = unfold null (f . head) tail

iitt :: (a -> a) -> a -> [a]
-- iitt f = unfold (null . (:[])) id f
iitt f = unfold (const False) id f

all1 p xs = and (map p xs)

-- all2 p xs = map p (and xs)

all3 p = and . map p

all4 p = not . any (not . p)

-- all5 p = map p . and 

all6 p xs = foldl (&&) True (map p xs)

-- no -- all7 p xs = foldr (&&) False (map p xs)

all8 p = foldr (&&) True . map p


-- any1 p = map p . or

any2 p = or . map p

any3 p xs = length (filter p xs) > 0

any4 p = not . null . dropWhile (not . p)

-- no -- any5 p = null . filter p

any6 p xs = not (all (\ x -> not (p x)) xs)

any7 p xs = foldr (\ x acc -> (p x) || acc) False xs

-- no -- any8 p xs = foldr (||) True (map p xs)

