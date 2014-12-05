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
-- foldl            :: (a -> b -> a) -> a -> [b] -> a
-- foldl f x []     =  x
-- foldl f x (y:ys) =  foldl f (f x y) ys

foldLeftM            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f x []     =  return x
foldLeftM f x (y:ys) =  do z  <- f x y
                           zs <- foldLeftM f z ys
                           return zs

--
-- recursive defn of foldr
--
-- foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr f x []     =  x
-- foldr f x (y:ys) =  f y (foldr f x ys)

foldRightM            :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f x []     =  return x
foldRightM f x (y:ys) = do zs <- foldRightM f x ys
                           z  <- f y zs
                           return z
--
-- liftM :: Monad m => (a -> b) -> m a -> m b
--

-- seems to work
liftM'1 :: Monad m => (a -> b) -> m a -> m b
liftM'1 f m
  = do x <- m
       return (f x)

-- Couldn't match type `IO' with `[]'
-- liftM'2 f m = m >>= \ a -> f a

-- seems to work
liftM'3 f m = m >>= \ a -> return (f a)

-- Couldn't match expected type `[Char]' with actual type `IO String'
-- liftM'4 f m = return (f m)

-- doubles something
-- liftM'5 f m = m >>= \ a -> m >>= \ b -> return (f a)

-- doubles something else
-- liftM'6 f m = m >>= \ a -> m >>= \ b -> return (f b)

-- Couldn't match expected type `[Char]' with actual type `IO String'
-- liftM'7 f m = mapM f [m]

-- Couldn't match expected type `[Char] -> a0'
-- liftM'8 f m = m >> \ a -> return (f a)
