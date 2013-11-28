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
mDim m = (z - x, y - w)
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

mRowRange m = [(mRowStart m)..(mRowEnd m)]
mColRange m = [(mColStart m)..(mColEnd m)]

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
                where subs = [(i :-> j, (mCell m i j) * c) | j <- mColRange m]

swap :: Int -> Int -> Matrix Int e -> Matrix Int e
swap c d m@(Matrix a) = Matrix $ a // subs
                where f n = if n == c then d else c
                      subs = [(n :-> k, mCell m (f n) k) | n <- [c, d],
                                                       k <- mColRange m]

saxpy :: Int -> Rational -> Int -> Matrix Int Rational -> Matrix Int Rational
saxpy i c j m@(Matrix a) = Matrix $ a // subs
                where subs = [(i :-> k, (mCell m i k) + (mCell m j k) * c)
                                        | k <- mColRange m]

row :: Int -> Matrix Int e -> [(Edge Int, e)]
row i m = [(i :-> j, mCell m i j) | j <- mColRange m]

col :: Int -> Matrix Int e -> [(Edge Int, e)]
col i m = [(j :-> i, mCell m j i) | j <- mRowRange m]


nan_row :: Int -> Matrix Int Rational -> Matrix Int Rational
nan_row i m
    | nz == []  = m  -- for a zero row, don't do anything
    | otherwise = foldl (\mat k -> if k == i 
                                   then mat 
                                   else saxpy k (-(mCell mat k j)) i mat)
                        (scale i (1 / (head nz)) m)
                        (mRowRange m)

        where (z, nz) = findFirstNZ 0 i m
              j = length z


gje :: Matrix Int Rational -> Matrix Int Rational
gje m = staircase (mRowStart m) $ foldl (\mat i -> nan_row i mat) m (mRowRange m)
        where staircase k = id

-- first param is an offset. so "find first non-zero occuring not 
-- before position k"
findFirstNZ :: Int -> Int -> Matrix Int Rational -> ([Rational], [Rational])
findFirstNZ k i = span (== 0) . drop k . map snd . row i


-- testing

d = [[0,4,7,9],[2,2,3,2],[0,1,6,5],[9,3,4,3]] :: [[Rational]]
e = mFL d
m = matrix e (0 :-> 0, 3 :-> 3)
nan1m = nan_row 1 m

mnorm i m = let (z,nz) = findFirstNZ 0 i m
            in scale i (1 / (head nz)) m

r0 = mnorm 1 m
r1 = saxpy 0 (-(mCell r0 0 0)) 1 r0
r2 = saxpy 2 (-(mCell r1 2 0)) 1 r1

c = [[1,4,7],[2,4,3],[3,1,6]] :: [[Rational]]
n = matrix (mFL c) (0 :-> 0, 2 :-> 2)

nan0n = nan_row 0 n

doofus = gje n
