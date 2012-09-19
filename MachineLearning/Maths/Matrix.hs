-- | A simple matrix module which supports basic matrix transformations such as
--   by-element and scalar arithmetic operations and matrix multiplication.
module MachineLearning.Maths.Matrix where

import MachineLearning.Helpers
import qualified Data.List as L


-----------------------------------------------------------------------------
-- * Constructors

data Matrix a = M { rows :: [[a]] } deriving (Eq)

instance (Show a) => Show (Matrix a) where
    show (M rs) = unlines' $ map show rs
    
instance (Read a) => Read (Matrix a) where
    readsPrec _ r = [(M $ map read $ lines r, [])]


-- | Block-matrix constructor.
--   The first value in the tuple is the number of rows, the second the number
--   of columns.
--
-- >>> (2, 3) <+> [1..]
-- [1, 2, 3]
-- [4, 5, 6]
(<+>) :: (Int, Int) -> [a] -> Matrix a
(<+>) (r, c) xs = M $ chunks' (take (r*c) xs) c

-- | Column vector constructor.
--
-- >>> 2 <|> [1..]
-- [1]
-- [2]
(<|>) :: Int -> [a] -> Matrix a
(<|>) r = (<+>) (r, 1) 

-- | Row vector constructor.
--
-- >>> 3 <-> [1..]
-- [1, 2, 3]
(<->) :: Int -> [a] -> Matrix a
(<->) r = (<+>) (1, r)

-- | Identity matrix constructor.
--
-- >>> (2, 3) <\> ('x', 'y')
-- ['y', 'x', 'x']
-- ['x', 'y', 'x']
(<\>) :: (Int, Int) -> (a, a) -> Matrix a
(<\>) (r, c) (zero, one) = M [   replicate (y - 1) zero
                              ++ [one] 
                              ++ replicate (c - y) zero | y <- [1..r] ]

-- | Creates a square numeric identity matrix
--
-- >>> mIdentity 2 :: Matrix Float
-- [1.0, 0.0]
-- [0.0, 1.0]
mIdentity :: (Num a) => Int -> Matrix a
mIdentity s = (s, s) <\> (0, 1)

-- | Creates a matrix with 0 elements.
mEmpty = (0,0) <+> []

-- | Creates a matrix with an infinite size.
mInfinite x = M (repeat $ repeat x)


-----------------------------------------------------------------------------
-- * Matrix Properties

-- | Gets the number of rows in a matrix.
numRows :: Matrix a -> Int
numRows m = length $ rows m

-- | Gets the number of columns in a matrix.
numCols :: Matrix a -> Int
numCols m | numRows m == 0 = 0
          | otherwise      = length $ head $ rows m

-- | Gets the number of cells in a matrix.
numElements :: Matrix a -> Int
numElements m = numRows m * numCols m

-- | Gets the size of a matrix.
size :: Matrix a -> (Int, Int)
size m = (numRows m, numCols m)

-- | Tests if the matrix is empty.
isEmpty :: (Eq a) => Matrix a -> Bool
isEmpty = (==) mEmpty

-----------------------------------------------------------------------------
-- * Sub-matrices

-- | Gets a list of columns of a matrix.
cols :: Matrix a -> [[a]]
cols m = rows $ mTranspose m

-- | Gets a single list with all elements of a matrix.
elements :: Matrix a -> [a]
elements m = concat $ rows m

-- | Gets the element in row r, column c.
element :: Matrix a -> (Int, Int) -> a
element m (r, c) = rows m !! r !! c

-- | Gets the i-th row of a matrix.
row :: Matrix a -> Int -> Matrix a
row m i | i >= 0 && i < numRows m = takeR 1 $ dropR i m
        | i < 0                   = error "Matrix.row: negative index"
        | otherwise               = error "Matrix.row: index too large"

-- | Gets the i-th column of a matrix.
col :: Matrix a -> Int -> Matrix a
col m i | i >= 0 && i < numCols m = takeC 1 $ dropC i m
        | i < 0                   = error "Matrix.col: negative index"
        | otherwise               = error "Matrix.col: index too large"

-- | Alternative to the element function.
(!!!) :: Matrix a -> (Int, Int) -> a
(!!!) = element

-- | Alternative to the row function.
(!-!) :: Matrix a -> Int -> Matrix a
(!-!) = row

-- | Alternative to the col function.
(!|!) :: Matrix a -> Int -> Matrix a
(!|!) = col

-- | Returns the upper-left value of the matrix.
mFirst :: Matrix a -> a
mFirst m = m !!! (0, 0)

-----------------------------------------------------------------------------

-- | Appends matrix b to matrix a as new rows.
(+-+) :: Matrix a -> Matrix a -> Matrix a
(+-+) a b | numElements a == 0 = b
          | numElements b == 0 = a
          | otherwise          = M $ rows a ++ rows b

-- | Appends matrix b to matrix a as new columns.
(+|+) :: Matrix a -> Matrix a -> Matrix a
(+|+) a b | numElements a == 0 = b
          | numElements b == 0 = a
          | otherwise          = M $ zipWith (++) (rows a) (rows b)

-- | Takes n rows from a matrix.
takeR :: Int -> Matrix a -> Matrix a
takeR n m = M $ take n $ rows m

-- | Drops n rows from a matrix.
dropR :: Int -> Matrix a -> Matrix a
dropR n m = M $ drop n $ rows m

-- | Takes n columns from a matrix.
takeC :: Int -> Matrix a -> Matrix a
takeC n m = mTranspose $ takeR n $ mTranspose m

-- | Drops n rows from a matrix.
dropC :: Int -> Matrix a -> Matrix a
dropC n m = mTranspose $ dropR n $ mTranspose m

-- | Repeats the rows of a matrix
repeatR :: Matrix a -> Matrix a
repeatR m = M $ concat $ repeat $ rows m

-- | Repeats the columns of a matrix
repeatC :: Matrix a -> Matrix a
repeatC m = mTranspose $ repeatR $ mTranspose m

-----------------------------------------------------------------------------
-- * Matrix Transformations

-- | Transpose the matrix.
mTranspose :: Matrix a -> Matrix a
mTranspose m = M $ L.transpose (rows m)

instance Functor Matrix where
    fmap f m = M $ map (map f) (rows m)
    
-- | Maps a function over each element in the matrix.
mMap :: (a -> b) -> Matrix a -> Matrix b
mMap = fmap

-- | Maps a function over each row in the matrix.
rMap :: ([a] -> [b]) -> Matrix a -> Matrix b
rMap f m = M $ map f (rows m)

-- | Maps a function over each column in the matrix.
cMap :: ([a] -> [b]) -> Matrix a -> Matrix b
cMap f m = mTranspose $ rMap f $ mTranspose m

-- | Zips two matrices with a given function f.
mZip :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mZip f a b = M $ zipWith (zipWith f) (rows a) (rows b)

-----------------------------------------------------------------------------
-- * Arithmetic operations

-- | By-element numerical operations (+), (-), (*).
instance (Num a) => Num (Matrix a) where
    (+)         = mZip (+)
    (-)         = mZip (-)
    (*)         = mZip (*)
    abs         = mMap abs
    signum      = error "signum is not applicable for Matrix"
    fromInteger = error "fromInteger is not applicable for Matrix"

-- | By-element fractional operations (/).
instance (Fractional a) => Fractional (Matrix a) where
    (/)          = mZip (/)
    fromRational = error "fromRational is not applicable for Matrix"

-----------------------------------------------------------------------------

-- | Scalar addition.
(!+) :: (Num a) => Matrix a -> a -> Matrix a
(!+) m x = mMap (+ x) m

-- | Scalar subtraction.
(!-) :: (Num a) => Matrix a -> a -> Matrix a
(!-) m x = mMap (subtract x) m

-- | Scalar multiplication.
(!*) :: (Num a) => Matrix a -> a -> Matrix a
(!*) m x = mMap (* x) m

-- | Scalar division.
(!/) :: (Fractional a) => Matrix a -> a -> Matrix a
(!/) m x = mMap (/ x) m

-----------------------------------------------------------------------------

-- | Matrix multiplication.
mMul :: (Num a) => Matrix a -> Matrix a -> Matrix a
mMul a b = rMap (\r -> map (sum . zipWith (*) r) (cols b)) a


-----------------------------------------------------------------------------
-- * Miscellaneous  functions

-- | Chops a matrix into column chunks of the given length.
--
-- >>> chunksC' ((2, 6) <+> [1..]) 3
-- [[1,2,3]
--  [7,8,9],
--  [4,5,6]
--  [10,11,12]]
chunksC' :: Matrix a -> Int -> [Matrix a]
chunksC' m i = map (mTranspose . M) $ chunks' (cols m) i

-----------------------------------------------------------------------------

--Test / Example matrices and vectors
m1 = (2, 2) <+> [1, 3, 2, 5]
v1 = 2 <|> [0,3]

m2 = (3, 2) <+> [1, 3, 2, 4, 0, 5]
v2 = 2 <|> [1, 2]

m3 = (2, 2) <+> [0, 1, 3, 2]
i1 = (2, 2) <\> (0, 1)