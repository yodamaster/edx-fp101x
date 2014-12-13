primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

rat :: Int -> Float
rat n = (fromIntegral $ last $ take n primes) / fromIntegral n

square :: Int -> Int
square n = n * n

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = (sumwith $! (v+x)) xs

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip (tail fibs) fibs]

mult :: Int -> Int -> Int
mult = \ x -> (\ y -> x * y)

largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node t x t
    where t = repeatTree x



