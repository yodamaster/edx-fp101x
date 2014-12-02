module Homework8 where

import Data.Char
import Control.Monad
import System.IO

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

strlen :: IO ()
strlen =
    do putStr "Enter a string: "
       xs <- getLine
       putStr "The string has "
       putStr (show (length xs))
       putStrLn " characters"

munch :: IO String
munch = return []

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putRev :: String -> IO ()
putRev [] = return ()
putRev (x:xs) = putRev xs >> putChar x



putStrLn'1 :: String -> IO ()
putStrLn'1 [] = putChar '\n'
putStrLn'1 xs = putStr' xs >> putStrLn'1 ""

putStrLn'2 :: String -> IO ()
putStrLn'2 [] = putChar '\n'
putStrLn'2 xs = putStr' xs >> putChar '\n'

putStrLn'3 :: String -> IO ()
putStrLn'3 [] = putChar '\n'
putStrLn'3 xs = putStr' xs >>= \ x -> putChar '\n'

-- putStrLn'4 :: String -> IO ()
-- putStrLn'4 [] = putChar '\n'
-- putStrLn'4 xs = putStr' xs >> \ x -> putChar '\n'

putStrLn'5 :: String -> IO ()
putStrLn'5 [] = putChar '\n'
putStrLn'5 xs = putStr' xs >> putStr' "\n"

-- no work -- putStrLn'6 :: String -> IO ()
-- no work -- putStrLn'6 [] = putChar '\n'
-- no work -- putStrLn'6 xs = putStr' xs >> putStrLn'6 "\n"

-- putStrLn'7 :: String -> IO ()
-- putStrLn'7 [] = return ""
-- putStrLn'7 xs = putStrLn'7 xs >> putStr' "\n"

-- putStrLn'8 :: String -> IO ()
-- putStrLn'8 [] = putChar "\n"
-- putStrLn'8 xs = putStr' xs >> putChar '\n'


getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs =
    do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f =
    do input <- getLine'
       putStrLn'1 (f input)


-- sequence_'1 :: Monad m => [m a] -> m ()
-- sequence_'1 [] = return []
-- sequence_'1 (m : ms) = m >> \ _ -> sequence_'1 ms

sequence_'2 :: Monad m => [m a] -> m ()
sequence_'2 [] = return ()
sequence_'2 (m : ms) = (foldl (>>) m ms) >> return ()

-- sequence_'3 :: Monad m => [m a] -> m ()
-- sequence_'3 ms = foldl (>>) (return ()) ms

sequence_'4 :: Monad m => [m a] -> m ()
sequence_'4 [] = return ()
sequence_'4 (m : ms) = m >> sequence_'4 ms

sequence_'5 :: Monad m => [m a] -> m ()
sequence_'5 [] = return ()
sequence_'5 (m : ms) = m >>= \ _ -> sequence_'5 ms

-- sequence_'6 :: Monad m => [m a] -> m ()
-- sequence_'6 ms = foldr (>>=) (return ()) ms

sequence_'7 :: Monad m => [m a] -> m ()
sequence_'7 ms = foldr (>>) (return ()) ms

-- sequence_'8 :: Monad m => [m a] -> m ()
-- sequence_'8 ms = foldr (>>) (return []) ms

--
-- recursive defn of foldl
--
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f a [] = a
-- foldl f a (x:xs) = foldl f (f a x) xs

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do z <- x
                          zs <- foldLeftM f a xs
                          return (z:zs)


