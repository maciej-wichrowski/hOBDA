module MachineLearning.Helpers where
import Numeric
import GHC.Float

-- | Unlines variant which does not add a newline after the last element.
unlines' :: [String] -> String
unlines' []     = []
unlines' (x:[]) = x
unlines' (x:xs) = x ++ '\n' : unlines' xs


-- | Chops a list into chunks of predefined lengths.
--
-- >>> chunks (repeat 42) [1, 2, 1, 3]
-- [[42], [42, 42], [42], [42, 42, 42]]
chunks :: [a] -> [Int] -> [[a]]
chunks _ []      = []
chunks [] _      = []
chunks xs (c:cs) = take c xs : chunks (drop c xs) cs


-- | Chops a list into equally sized chunks.
--  
-- >>> chunks' [42, 42, 42, 42, 42] 2
-- [[42, 42], [42, 42], [42]]
chunks' :: [a] -> Int -> [[a]]
chunks' xs i = chunks xs (repeat i)


-- | Shows a Float with the least number of decimals possible, with a maximum 
--   of d decimals.
--
-- >>> showFFloat' 3 2.0000001 
-- "2"
-- >>> showFFloat' 3 2.1234567 
-- "2.123"
-- >>> showFFloat' 3 10.000001
-- "10"
showFFloat' :: Int -> Float -> String
showFFloat' d f = wholeNumbers ++ removeTrailingZeros (take (d + 1) decimals)
    where 
        (wholeNumbers, decimals) = break ('.' ==) $ showFFloat Nothing f ""
        removeTrailingZeros []                  = []
        removeTrailingZeros xs | last xs == '0' = removeTrailingZeros (init xs)
                               | last xs == '.' = removeTrailingZeros (init xs)
                               | otherwise      = xs

showFDouble' :: Int -> Double -> String
showFDouble' d f = showFFloat' d (double2Float f)