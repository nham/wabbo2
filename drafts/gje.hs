import Data.Ratio ((%))

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace i v xs = a ++ (v : tail b) 
                where (a, b) = splitAt i xs

type Vector = [Rational]
type Matrix = [Vector]

-- vector helpers
addv :: Vector -> Vector -> Vector
addv [] [] = []
addv (u:us) (v:vs) = (u+v) : addv us vs

smultv :: Rational -> Vector -> Vector
smultv c = map (*c)


-- elementary row operations

scale :: Int -> Rational -> Matrix -> Matrix
scale i c m = a ++ (smultv c (head b) : tail b)
                where (a, b) = splitAt i m

swap :: Int -> Int -> Matrix -> Matrix
swap i j m = let row_i = m !! i
                 row_j = m !! j
             in replace j row_i $ replace i row_j m

saxpy :: Int -> Rational -> Int -> Matrix -> Matrix
saxpy i c j m = (a ++) $ addv (head b) (smultv c row_j) : tail b
                    where (a, b) = splitAt i m
                          row_j = m !! j


gj_noswap :: Matrix -> Matrix
gj_noswap = map vnlnz


-- for a Vector, Normalize the Leading NonZero component
vnlnz :: Vector -> Vector
vnlnz v = a ++ smultv (1 / (head b)) b
            where (a, b) = span (== 0) v

-- given a row i, if column j is the first non-zero component of row i,
-- zero out the rest of the column
annihilate_col :: Int -> Matrix -> Matrix
annihilate_col i m = 
    if b == [] then m
    else let j = length a
             c = head b
             nihil k l mat
                 | k == l    = mat
                 | k == i    = nihil (k+1) l mat
                 | otherwise = nihil (k+1) l $ saxpy k
                                                     (-(mat !! k !! j) / c)
                                                     1 
                                                     mat
         in nihil 0 (length m) m

      where (a, b) = span (== 0) $ m !! i
