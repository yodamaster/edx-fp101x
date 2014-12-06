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


