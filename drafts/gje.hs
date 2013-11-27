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
gj_noswap m = gje 0 (length m) m
        where gje k l mat
                | k == l    = mat
                | otherwise = gje (k+1) l . nan_row k $ mat


-- normalize a row so that the leading nonzero, if it exists, is 1
normalize_row :: Int -> Matrix -> Matrix
normalize_row i mat = let a = dropWhile (== 0) $ mat !! i
                      in if a == []
                         then mat
                         else scale i (1 / (head a)) mat

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
                                                     i 
                                                     mat
         in nihil 0 (length m) m

      where (a, b) = span (== 0) $ m !! i


-- normalize and annihilate row
nan_row :: Int -> Matrix -> Matrix
nan_row i m = 
    if b == [] then m
    else let j = length a
             c = head b
             nihil k l mat
                 | k == l    = mat
                 | k == i    = nihil (k+1) l mat
                 | otherwise = nihil (k+1) l $ saxpy k
                                                     (-(mat !! k !! j))
                                                     i 
                                                     mat
         in nihil 0 (length m) $ scale i (1 / c) m

      where (a, b) = span (== 0) $ m !! i


showmat :: Matrix -> String
showmat = foldl (\x y-> x ++ show y ++ "   ") ""


vIsZero :: Vector -> Bool
vIsZero = foldl f True
            where f False _ = False
                  f _ 0 = True
                  f _ _ = False
