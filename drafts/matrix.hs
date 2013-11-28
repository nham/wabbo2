import Data.Array

data Edge i = i :-> i deriving (Eq, Ord, Bounded, Ix, Show)
newtype Matrix i e = Matrix (Array (Edge i) e) deriving (Show)

matrix :: (Ix i, Bounded i) => (Edge i -> e) -> (Edge i, Edge i) -> Matrix i e
matrix f bp = Matrix . listArray bp . map f $ range bp

entireRange :: (Ix i, Bounded i) => [i]
entireRange = range (minBound, maxBound)

indexOf :: (Ix i, Bounded i) => i -> Int
indexOf = index (minBound, maxBound)

mDim :: (Ix i) => Matrix i e -> Edge i
mDim (Matrix a) = snd $ bounds a
mHgt m = let (a :-> b) = mDim m
         in a
mWid m = let (a :-> b) = mDim m
         in b

-- matrix From List. not really accurate since it actually converts a 2d-list
-- into a function that can be submitted to "matrix" function
mFL :: [[Rational]] -> (Edge Int) -> Rational
mFL l (i :-> j) = l !! i !! j

scale :: Int -> Rational -> Matrix Int Rational -> Matrix Int Rational
scale i c m@(Matrix a) = Matrix $ a // subs
                where subs = [(i :-> j, a ! (i :-> j) * c) | j <- [0..(mWid m)]]

swap :: Int -> Int -> Matrix Int e -> Matrix Int e
swap c d m@(Matrix a) = Matrix $ a // subs
                where f n = if n == c then d else c
                      g n k = f n :-> k
                      subs = [(n :-> k, a ! (g n k)) | n <- [c, d],
                                                       k <- [0..(mWid m)]]
