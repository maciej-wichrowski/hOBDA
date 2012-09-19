-- | A collection of statistical operations on matrices.
module MachineLearning.Maths.Statistics where
import MachineLearning.Maths.Matrix


-----------------------------------------------------------------------------
-- * Sum and mean values

-- | Sums up all rows and returns the sums as a column vector
rSum :: (Num a) => Matrix a -> Matrix a
rSum = rMap (\r -> [sum r])

-- | Sums up all columns and returns the sums as a row vector
cSum :: (Num a) => Matrix a -> Matrix a
cSum = cMap (\c -> [sum c])

-- | Sums up all values in a matrix.
mSum :: (Num a) => Matrix a -> a
mSum m = mFirst $ (cSum . rSum) m


-----------------------------------------------------------------------------


-- | Calculates the mean value of each row in a matrix.
rMean :: (Floating a) => Matrix a -> Matrix a
rMean m = rSum m !* (1 / fromIntegral (numCols m))

-- | Calculates the mean value of each column in a matrix.
cMean :: (Floating a) => Matrix a -> Matrix a
cMean m = mTranspose $ rMean $ mTranspose m

-- | Calculates the mean over of values in a matrix.
mMean :: (Floating a) => Matrix a -> a
mMean m = 1 / fromIntegral (numElements m) * mSum m


-----------------------------------------------------------------------------
-- * Quantile

-- | Gets a given quantile from a list of values.
--
-- >>> quantile 0.25 [1,2,3,4,5]
-- 2.0
-- >>> quantile 0.75 [1,2,3,4,5]
-- 4.0
quantile :: (RealFrac a) => a -> [a] -> a
quantile q xs | isWhole l = 0.5 * ((xs !! (round l - 1)) + xs !! round l)
              | otherwise = xs !! (ceiling l - 1)
    where numXs     = length xs
          l         = fromIntegral numXs * q
          isWhole x = x == fromInteger (round x)


-- | Gets the median from a list of values.
median :: (RealFrac a) => [a] -> a
median = quantile 0.5


-- | Gets three values dividing a list into quartiles. (Q1, median, Q3).
--
-- >>> quartiles [1,2,3,4]
-- (1.5,2.5,3.5)
quartiles :: (RealFrac a) => [a] -> (a, a, a)
quartiles xs = (quantile 0.25 xs, median xs, quantile 0.75 xs)

-- | Gets the five-number summary from a list. (min, Q1, median, Q3, max).
--
-- >>> fiveNum [1,2,3,4]
-- (1.0,1.5,2.5,3.5,4.0)
fiveNum :: (RealFrac a) => [a] -> (a, a, a, a, a)
fiveNum xs = (minimum xs, q1, m, q3, maximum xs)
    where (q1, m, q3) = quartiles xs

          
-----------------------------------------------------------------------------
-- * Standard Deviation and Normalization

-- | Returns a column vector with the standard deviation of each row in a matrix.
rStdDev :: (Floating a) => Matrix a -> Matrix a
rStdDev m = M (map (\r -> [sqrt (sum r / fromIntegral (numCols m))]) diffSq)
    where means  = rMean m
          diff   = zipWith (zipWith (-)) (rows m) [repeat (head mr) | mr <- rows means]
          diffSq = map (map (^2)) diff

-- | Returns a row vector with the standard deviation over all rows.
cStdDev :: (Floating a) => Matrix a -> Matrix a
cStdDev m = mTranspose $ rStdDev $ mTranspose m

-- | Normalizes each row in a matrix.
--
--   Returns a triple: (Matrix normalized over columns, means column vector, std. dev. column vector).
rNorm :: (Floating a) => Matrix a -> (Matrix a, Matrix a, Matrix a)
rNorm m = (M scaled, means, stdDev)
    where means  = rMean m
          stdDev = rStdDev m
          transl = zipWith (zipWith (-)) (rows m) [repeat (head mr) | mr <- rows means]
          scaled = zipWith (zipWith (/)) transl [repeat (head sr) | sr <- rows stdDev]

-- | Normalizes each column in a matrix.
cNorm :: (Floating a) => Matrix a -> (Matrix a, Matrix a, Matrix a)
cNorm m = (mTranspose std, mTranspose means, mTranspose stdDev)
    where (std, means, stdDev) = rNorm (mTranspose m)


-----------------------------------------------------------------------------
-- * Mean Squared Error

-- | Mean squared error.
--
-- > mse(x,y) = 1/2n * sum( (x(i) - y(i)) ^ 2)
mse :: (Floating a) => Matrix a -> Matrix a -> a
mse xs ys = mMean (mMap (^ 2) (xs - ys))


-- | Root mean squared error.
rmse :: (Floating a) => Matrix a -> Matrix a -> a
rmse xs ys = sqrt $ mse xs ys


-----------------------------------------------------------------------------
-- * Correlations

-- | Pearson's correlation between columns x and y in matrix m.
--
-- > corr(X,Y) = cov(X,Y) / (stdDev(X) stdDev(Y))
corr :: (Floating a) => Matrix a -> Int -> Int -> a
corr m x y = covariance mX mY / ( mFirst (cStdDev mX) * mFirst (cStdDev mY) )
    where mX = m !|! x
          mY = m !|! y
       
-- | Covariance between two column-vectors.
--
-- > q(jk) = 1 / (n - 1) * sum (x(ij) - mean( x(j) )) * ( x(ik) - mean( x(j) ) )
covariance :: (Floating a) => Matrix a -> Matrix a -> a
covariance xs ys = (1 / (n - 1)) * mSum (xDiff * yDiff)
    where n = fromIntegral $ numRows xs
          xMean = mMean xs
          yMean = mMean ys
          xDiff = mMap (\x -> x - xMean) xs
          yDiff = mMap (\y -> y - yMean) ys