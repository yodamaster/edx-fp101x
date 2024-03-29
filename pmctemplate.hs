module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

-- *Lab5> :t Stop
-- Stop :: Action
-- *Lab5> :t const Stop
-- const Stop :: b -> Action
action :: Concurrent a -> Action
action = \ (Concurrent x) -> x (const Stop)


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\ _ -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
-- atom = \ c -> Concurrent (\ k -> Atom (fmap k c))
atom k = Concurrent $ \ a -> Atom (k >>= \ c -> return (a c))

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork = \ a -> Concurrent (\ k -> Fork (action a) (k ()))


par :: Concurrent a -> Concurrent a -> Concurrent a
par a1 a2 = Concurrent (\ k -> Fork (action a1) (action a2))



-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent $ \ c -> f (\ k -> case g k of (Concurrent t) -> t c)
    return x = Concurrent (\ c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = \ as -> case as of
                        [] -> return ()
                        (Atom a : xs) -> a >>= \inp -> roundRobin (xs ++ [inp])
                        (Fork a1 a2 : xs) -> roundRobin (xs ++ [a1,a2])
                        (Stop : xs) -> roundRobin xs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

