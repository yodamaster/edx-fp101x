module Lab6 where

------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- *Lab6> :t (:>)
-- (:>) :: a -> [Rose a] -> Rose a

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (p :> cs) = p

children :: Rose a -> [Rose a]
children (p :> cs) = cs

tree0 = 'x' :> map (flip (:>) []) ['a'..'x']
ex0 = length $ children tree0

tree1 = 'x' :> map (\c -> c :> []) ['a'..'A']
ex1 = length (children tree1)

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (p :> cs) = 1 + (sum $ map size cs)

leaves :: Rose a -> Int
leaves (p :> []) = 1
leaves (p :> cs) = sum $ map leaves cs

tree3 = 1 :> map (\c -> c :> []) [1..5]
ex3 = size tree3

tree4 = 1 :> map (\c -> c :> []) [1..5]
ex4 = size . head . children $ tree4

tree5 = 1 :> map (\c -> c :> []) [1..5]
ex5 = leaves tree5

tree6 = 1 :> map (\c -> c :> []) [1..5]
ex6 = product (map leaves (children tree6))

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (p :> cs) = f p :> map (fmap f) cs

tree8 = 1 :> map (\c -> c :> []) [1..5]
ex8 = size (fmap leaves (fmap (:> []) tree8))

f :: Rose a -> Rose a
f r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

unSum :: Sum a -> a
unSum (Sum x) = x
unProduct :: Product a -> a
unProduct (Product x) = x

ex11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))


-- ===================================
-- Ex. 14-15
-- ===================================

-- mempty

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap = error "eek"

instance Foldable Rose where
  fold (m :> cs) = mappend m (foldr (mappend . fold) mempty cs)

tree14 = 1 :> [2 :> [], 3 :> [4 :> []]]
tree14' = fmap Product tree14
ex14 = unProduct $ fold tree14'

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

tree16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum = error "you have to implement fsum"
fproduct = error "you have to implement fproduct"

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

