module Chapter9 where

import Data.Char
import Control.Monad
import System.IO
import Parsing

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
munch = return "dumb"

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


sequence'1 :: Monad m => [m a] -> m [a]
sequence'1 [] = return []
sequence'1 (m : ms) =
  m >>=
    \ a ->
      do as <- sequence'1 ms
         return (a : as)

-- sequence'2 :: Monad m => [m a] -> m [a]
-- sequence'2 ms = foldr func (return ()) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc
--       = do x <- m
--            xs <- acc
--            return (x : xs)

-- sequence'3 :: Monad m => [m a] -> m [a]
-- sequence'3 ms = foldr func (return []) ms
--   where
--     func :: (Monad m) => m a -> m [a] -> m [a]
--     func m acc = m : acc

-- sequence'4 :: Monad m => [m a] -> m [a]
-- sequence'4 [] = return []
-- sequence'4 (m : ms) = return (a : as)
--     where
--         a <- m
--         as <- sequence'4 ms

sequence'5 :: Monad m => [m a] -> m [a]
sequence'5 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc =
      do x <- m
         xs <- acc
         return (x : xs)

-- sequence'6 :: Monad m => [m a] -> m [a]
-- sequence'6 [] = return []
-- sequence'6 (m : ms)
--   = m >>
--       \ a ->
--         do as <- sequence'6 ms
--            return (a : as)

-- sequence'7 :: Monad m => [m a] -> m [a]
-- sequence'7 [] = return []
-- sequence'7 (m : ms) = m >>= \a ->
--   as <- sequence'7 ms
--   return (a : as)

sequence'8 :: Monad m => [m a] -> m [a]
sequence'8 [] = return []
sequence'8 (m : ms) =
  do a <- m
     as <- sequence'8 ms
     return (a : as)


mapM'1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'1 f as = sequence'8 (map f as)

mapM'2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'2 f [] = return []
mapM'2 f (a : as)
  = f a >>= \ b -> mapM'2 f as >>= \ bs -> return (b : bs)

-- mapM'3 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM'3 f as = sequence_'7 (map f as)

-- mapM'4 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM'4 f [] = return []
-- mapM'4 f (a : as)
--   = f a >> \ b -> mapM'4 f as >> \ bs -> return (b :bs)

-- mapM'5 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM'5 f [] = return []
-- mapM'5 f (a : as) =
--     do
--          f a -> b
--          mapM'5 f as -> bs
--          return (b : bs)

mapM'6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'6 f [] = return []
mapM'6 f (a : as)
  = do b <- f a
       bs <- mapM'6 f as
       return (b : bs)

mapM'7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'7 f [] = return []
mapM'7 f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM'7 f as
           return (b : bs)

mapM'8 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'8 f [] = return []
mapM'8 f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM'8 f as
           return (bs ++ [b])

filterM'1 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM'1 _ [] = return []
filterM'1 p (x : xs)
  = do flag <- p x
       ys <- filterM'1 p xs
       return (x : ys)

filterM'2 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM'2 _ [] = return []
filterM'2 p (x : xs)
  = do flag <- p x
       ys <- filterM'2 p xs
       if flag then return (x : ys) else return ys

-- filterM'3 :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM'3 _ [] = return []
-- filterM'3 p (x : xs)
--   = do ys <- filterM'3 p xs
--        if p x then return (x : ys) else return ys

filterM'4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM'4 _ [] = return []
filterM'4 p (x : xs)
  = do flag <- p x
       ys <- filterM'4 p xs
       if flag then return ys else return (x : ys)

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) =
	putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a 
                 seqn as

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where
          	standard = "qcd=123+456-789*0()/"
          	extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1,y) xs | (y,xs) <- zip [1..13] box]

display :: String -> IO ()
display xs = do writeat (3,2) "             "
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
                 process c xs
               else
               	 do beep
               	    calc xs

process :: Char -> String -> IO ()
process c xs 
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
	         [(n,"")] -> calc (show n)
	         _ -> do beep
	                 calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

