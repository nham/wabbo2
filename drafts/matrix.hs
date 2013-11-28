import Data.Array ((!), bounds, listArray, Array, (//), Ix, range, index)
import Data.Ratio ((%))

data Edge i = i :-> i deriving (Eq, Ord, Bounded, Ix, Show)
newtype Matrix i e = Matrix (Array (Edge i) e) deriving (Show)

matrix :: (Ix i, Bounded i) => (Edge i -> e) -> (Edge i, Edge i) -> Matrix i e
matrix f bp = Matrix . listArray bp . map f $ range bp

entireRange :: (Ix i, Bounded i) => [i]
entireRange = range (minBound, maxBound)

indexOf :: (Ix i, Bounded i) => i -> Int
indexOf = index (minBound, maxBound)

mDim :: Matrix Int e -> (Int, Int)
mDim m@(Matrix a) = (z - x, y - w)
                        where (w :-> x) = mStart m
                              (y :-> z) = mEnd m

mStart, mEnd :: Matrix Int e -> Edge Int
mStart (Matrix a) = fst $ bounds a
mEnd (Matrix a) = snd $ bounds a

mRowStart, mRowEnd, mColStart, mColEnd :: Matrix Int e -> Int
mRowStart m = let (x :-> y) = mStart m in x
mRowEnd   m = let (x :-> y) = mEnd m in x
mColStart m = let (x :-> y) = mStart m in y
mColEnd   m = let (x :-> y) = mEnd m in y

mHgt, mWid :: Matrix Int e -> Int
mHgt = fst . mDim
mWid = snd . mDim

-- the regular notation is a bit unreadable.
mCell m@(Matrix a) i j = a ! (i :-> j)

-- matrix From List. not really accurate since it actually converts a 2d-list
-- into a function that can be passed to "matrix" function
mFL :: [[Rational]] -> (Edge Int) -> Rational
mFL l (i :-> j) = l !! i !! j

scale :: Int -> Rational -> Matrix Int Rational -> Matrix Int Rational
scale i c m@(Matrix a) = Matrix $ a // subs
                where subs = [(i :-> j, (mCell m i j) * c) | j <- [0..(mWid m)]]

swap :: Int -> Int -> Matrix Int e -> Matrix Int e
swap c d m@(Matrix a) = Matrix $ a // subs
                where f n = if n == c then d else c
                      subs = [(n :-> k, mCell m (f n) k) | n <- [c, d],
                                                       k <- [0..(mWid m)]]

saxpy :: Int -> Rational -> Int -> Matrix Int Rational -> Matrix Int Rational
saxpy i c j m@(Matrix a) = Matrix $ a // subs
                where subs = [(i :-> k, (mCell m i k) + (mCell m j k) * c)
                                        | k <- [0..(mWid m)]]

row :: Int -> Matrix Int e -> [(Edge Int, e)]
row i m = [(i :-> j, mCell m i j) | j <- [0..(mWid m)]]

col :: Int -> Matrix Int e -> [(Edge Int, e)]
col i m = [(j :-> i, mCell m j i) | j <- [0..(mHgt m)]]


nan_row :: Int -> Matrix Int Rational -> Matrix Int Rational
nan_row i m
    | nz == []  = m  -- for a zero row, don't do anything
    | otherwise = nihil i j end start $ scale i (1 / (head nz)) m

        where (z, nz) = findFirstNZ i m
              j = length z
              start = mRowStart m
              end = mRowEnd m


nihil :: Int -> Int -> Int -> Int -> Matrix Int Rational -> Matrix Int Rational
nihil row col end k mat = nih k mat
    where nih k mat
            | k == end  = mat
            | k == row  = nih (k+1) mat
            | otherwise = nih (k+1) $ saxpy k (-(mCell mat k col)) row mat


findFirstNZ :: Int -> Matrix Int Rational -> ([Rational], [Rational])
findFirstNZ i = span (== 0) . map snd . row i

-- for each row j that isnt i, saxpy j (-cj) i, where cj = value in that column



d = [[1,4,7,9],[2,2,3,2],[0,1,6,5],[9,3,4,3]] :: [[Rational]]
e = mFL d
m = matrix e (0 :-> 0, 3 :-> 3)
nan1m = nan_row 1 m

mnorm i m = let (z,nz) = findFirstNZ i m
            in scale i (1 / (head nz)) m

r0 = mnorm 1 m
r1 = saxpy 0 (-(mCell r0 0 0)) 1 r0
r2 = saxpy 2 (-(mCell r1 2 0)) 1 r1

c = [[1,4,7],[2,4,3],[3,1,6]] :: [[Rational]]
n = matrix (mFL c) (0 :-> 0, 2 :-> 2)

nan1n = nan_row 1 n
s0 = mnorm 1 n
