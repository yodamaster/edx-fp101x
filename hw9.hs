{-# LANGUAGE NPlusKPatterns #-}

module Homework9 where

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

-- works for Zero and (Succ(Succ...Zero))
natToInteger'1 :: Nat -> Integer
natToInteger'1 Zero = 0
natToInteger'1 (Succ n) = natToInteger'1 n + 1

-- works for Zero and (Succ(Succ...Zero))
natToInteger'2 :: Nat -> Integer
natToInteger'2 (Succ n) = natToInteger'2 n + 1
natToInteger'2 Zero = 0

-- never terminates
-- natToInteger'3 :: Nat -> Integer
-- natToInteger'3 n = natToInteger'3 n

-- works for Zero and (Succ(Succ...Zero))
natToInteger'4 :: Nat -> Integer
natToInteger'4 (Succ n) = 1 + natToInteger'4 n
natToInteger'4 Zero = 0

-- always returns 1
-- natToInteger'5 :: Nat -> Integer
-- natToInteger'5 Zero = 1
-- natToInteger'5 (Succ n) = (1 + natToInteger'5 n) - 1

-- works for Zero and (Succ(Succ...Zero))
natToInteger'6 :: Nat -> Integer
natToInteger'6 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

-- works for Zero and (Succ(Succ...Zero))
natToInteger'7 :: Nat -> Integer
natToInteger'7 = \ n -> genericLength [c | c <- show n, c == 'S']

-- Couldn't match expected type `Integer' with actual type `Int'
-- natToInteger'8 :: Nat -> Integer
-- natToInteger'8 = \ n -> length [c | c <- show n, c == 'S']

-- works for 0 and 5
integerToNat'1 :: Integer -> Nat
integerToNat'1 0 = Zero
integerToNat'1 (n+1) = Succ (integerToNat'1 n)

-- for 0, gives wrong answer; otherwise never terminates
-- integerToNat'2 :: Integer -> Nat
-- integerToNat'2 0 = Succ Zero
-- integerToNat'2 n = (Succ (integerToNat'2 n))

-- Couldn't match expected type ‘Nat’ with actual type ‘Integer’
-- integerToNat'3 :: Integer -> Nat
-- integerToNat'3 n
--   = product [(unsafeCoerce c) :: Integer | c <- show n]

-- never terminates
-- integerToNat'4 :: Integer -> Nat
-- integerToNat'4 n = integerToNat'4 n

-- works for 0 and 5
integerToNat'5 :: Integer -> Nat
integerToNat'5 (n+1) = Succ (integerToNat'5 n)
integerToNat'5 0 = Zero

-- works for 0 and 5
integerToNat'6 :: Integer -> Nat
integerToNat'6 (n+1) = let m = integerToNat'6 n in Succ m
integerToNat'6 0 = Zero

-- Occurs check: cannot construct the infinite type: t ~ [t]
-- integerToNat'7 :: Integer -> Nat
-- integerToNat'7 = head . m
--   where {
--   		; m 0 = [0]
--   		; m (n+1) = [sum x | x <- (1 : m n)]
--         }

-- No instance for (Num Nat) arising from a use of ‘genericLength’
-- integerToNat'8 :: Integer -> Nat
-- integerToNat'8 = \ n -> genericLength [c | c <- show n, isDigit c]

-- works for Zero n, n Zero, n m
add'1 :: Nat -> Nat -> Nat
add'1 Zero n = n
add'1 (Succ m) n = Succ (add'1 n m)

-- works for Zero n, n Zero, n m
add'2 :: Nat -> Nat -> Nat
add'2 (Succ m) n = Succ (add'2 n m)
add'2 Zero n = n

-- wrong answer for Zero n
-- add'3 :: Nat -> Nat -> Nat
-- add'3 Zero n = Zero
-- add'3 (Succ m) n = Succ (add'3 m n)

-- wrong answer for Zero n
-- add'4 :: Nat -> Nat -> Nat
-- add'4 (Succ m) n = Succ (add'4 m n)
-- add'4 Zero n = Zero

-- wrong answer for n Zero
-- add'5 :: Nat -> Nat -> Nat
-- add'5 n Zero = Zero
-- add'5 n (Succ m) = Succ (add'5 n m)

-- wrong answer for n Zero
-- add'6 :: Nat -> Nat -> Nat
-- add'6 n (Succ m) = Succ (add'6 n m)
-- add'6 n Zero = Zero

-- works for Zero n, n Zero, n m
add'7 :: Nat -> Nat -> Nat
add'7 n Zero = n
add'7 n (Succ m) = Succ (add'7 m n)

-- works for Zero n, n Zero, n m
add'8 :: Nat -> Nat -> Nat
add'8 n (Succ m) = Succ (add'8 m n)
add'8 n Zero = n

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add'1 m (mult m n)

data Tree = Leaf Integer
          | Node Tree Integer Tree

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- works and (map (\ n -> occurs'1 n t) [1,3,4,5,6,7,9])
occurs'1 :: Integer -> Tree -> Bool
occurs'1 m (Leaf n) = m == n
occurs'1 m (Node l n r)
  = case compare m n of
        LT -> occurs'1 m l
        EQ -> True
        GT -> occurs'1 m r

-- only works for 5
-- occurs'2 :: Integer -> Tree -> Bool
-- occurs'2 m (Leaf n) = m == n
-- occurs'2 m (Node l n r)
--   = case compare m n of 
--   		LT -> occurs'2 m r
--   		EQ -> True
--   		GT -> occurs'2 m l

-- Couldn't match expected type ‘Bool’ with actual type ‘Ordering’
-- occurs'3 :: Integer -> Tree -> Bool
-- occurs'3 m (Leaf n) = compare m n
-- occurs'3 m (Node l n r)
--   = case compare m n of
--   		LT -> occurs'3 m l
--   		EQ -> True
--   		GT -> occurs'3 m r

-- only works for odd positions
-- occurs'4 :: Integer -> Tree -> Bool
-- occurs'4 m (Leaf n) = m == n
-- occurs'4 m (Node l n r)
--   = case compare m n of
--   		LT -> occurs'4 m l
--   		EQ -> False
--   		GT -> occurs'4 m r

-- works and (map (\ n -> occurs'5 n t) [1,3,4,5,6,7,9])
occurs'5 :: Integer -> Tree -> Bool
occurs'5 m (Leaf n) = m == n
occurs'5 m (Node l n r)
  | m == n = True
  | m < n = occurs'5 m l
  | otherwise = occurs'5 m r

-- only works for 5
-- occurs'6 :: Integer -> Tree -> Bool
-- occurs'6 m (Leaf n) = m == n
-- occurs'6 m (Node l n r)
--   | m == n = True
--   | m > n = occurs'6 m l
--   | otherwise = occurs'6 m r

-- Couldn't match expected type ‘Integer’ with actual type ‘Tree’
-- occurs'7 :: Integer -> Tree -> Bool
-- occurs'7 m n = m == n
-- occurs'7 m (Node l n r)
--   | m == n = True
--   | m < n = occurs'7 m l
--   | otherwise = occurs'7 m r

-- Couldn't match expected type ‘Integer’ with actual type ‘Tree’
-- occurs'8 :: Integer -> Tree -> Bool
-- occurs'8 m n = m == n
-- occurs'8 m (Node l n r)
--   | m == n = False
--   | m < n = occurs'8 m r
--   | otherwise = occurs'8 m l

data Tree' = Leaf' Integer
           | Node' Tree' Tree'

t'1 :: Tree'
t'1 = Node' (Node' (Leaf' 1) (Leaf' 4)) (Node' (Leaf' 6) (Leaf' 9))
t'2 :: Tree'
t'2 = Node' (Node' (Node' (Leaf' 1) (Leaf' 4)) (Leaf' 5)) (Leaf' 7)

-- does not work on t'2; returns True when should return False
-- balanced'1 :: Tree' -> Bool
-- leaves'1 (Leaf' x) = x
-- leaves'1 (Node' l r) = leaves'1 l + leaves'1 r
-- balanced'1 (Leaf' _) = True
-- balanced'1 (Node' l r)
--   = abs (leaves'1 l - leaves'1 r) <= 1 || balanced'1 l || balanced'1 r

-- No instance for (Num Bool) arising from a use of ‘+’
-- balanced'2 :: Tree' -> Bool
-- leaves'2 (Leaf' _) = True
-- leaves'2 (Node' l r) = leaves'2 l + leaves'2 r
-- balanced'2 (Leaf' _) = True
-- balanced'2 (Node' l r) = abs (leaves'2 l - leaves'2 r) <= 1

-- No instance for (Num Bool) arising from a use of ‘+’
-- balanced'3 :: Tree' -> Bool
-- leaves'3 (Leaf' _) = True
-- leaves'3 (Node' l r) = leaves'3 l + leaves'3 r
-- balanced'3 (Leaf' _) = True
-- balanced'3 (Node' l r) = abs (leaves'3 l + leaves'3 r) <= 1

balanced'4 :: Tree' -> Bool
leaves'4 (Leaf' _) = 1
leaves'4 (Node' l r) = leaves'4 l + leaves'4 r
balanced'4 (Leaf' _) = True
balanced'4 (Node' l r) 
  = abs (leaves'4 l - leaves'4 r) <= 1 && balanced'4 l && balanced'4 r

balance'1 :: [Integer] -> Tree'
halve'1 xs = splitAt (length xs `div` 2) xs
balance'1 [x] = Leaf' x
balance'1 xs = Node' (balance'1 ys) (balance'1 zs)
  where (ys, zs) = halve'1 xs

-- No instance for (Fractional Int) arising from a use of ‘/’
-- balance'2 :: [Integer] -> Tree'
-- halve'2 xs = splitAt (length xs / 2) xs
-- balance'2 [x] = Leaf' x
-- balance'2 xs = Node' (balance'2 ys) (balance'2 zs)
--   where (ys, zs) = halve'2 xs

-- Couldn't match expected type ‘(t, t1)’ with actual type ‘Tree'’
-- balance'3 :: [Integer] -> Tree'
-- halve'3 xs = splitAt (length xs `div` 2) xs
-- balance'3 [x] = Leaf' x
-- balance'3 xs = Node' ys zs
--   where (ys, zs) = balance'3 (halve'3 xs)

-- Couldn't match expected type ‘Integer’ with actual type ‘[Integer]’
-- balance'4 :: [Integer] -> Tree'
-- halve'4 xs = splitAt (length xs `div` 2) xs
-- balance'4 x = Leaf' x
-- balance'4 xs = Node' (balance'4 ys) (balance'4 zs)
--   where (ys, zs) = halve'4 xs

data Maybe' a = Nothing' | Just' a deriving Show
instance Monad Maybe' where
        return x = Just' x
        Nothing' >>= _ = Nothing'
        (Just' x) >>= f = f x

data Maybe'' a = Nothing'' | Just'' a deriving Show
instance Functor Maybe'' where
	    fmap _ Nothing'' = Nothing''
	    fmap f (Just'' a) = Just'' (f a)