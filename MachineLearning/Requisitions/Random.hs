module MachineLearning.Requisitions.Random (
   -- * Default values
     defaultFMin
   , defaultFMax
   -- * Generating new sets
   , generateSet
   , generateSet'
   , generateFeatures
   , generateFeatures'
   , generatePicks
   -- * Inserting errors
   , insertErrors
   -- * Helpers
   , randRangeExcept
   , randOneOf
   , uniqueRandList
) where
import MachineLearning.Requisitions.Requisitions
import MachineLearning.Maths.Matrix
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe
import Text.Printf


----------------------------------------------------------------------------


-- | The default minimum feature value.
defaultFMin = 0

-- | The default maximum feature value.
defaultFMax = 100


----------------------------------------------------------------------------


-- | Generates a random training set.
generateSet :: Int -- ^ The number of orders to generate.
            -> Int -- ^ The number of suppliers per order.
            -> Int -- ^ The number of features per supplier.
            -> [Int] -- ^ The feature to base picking the best supplier on.
            -> IO TrainingSet
generateSet numOrders numSuppliers numFeatures = 
   generateSet' numOrders numSuppliers numFeatures defaultFMin defaultFMax
      
      
-- | Generates a random training set.
generateSet' :: Int -- ^ The number of orders to generate.
            -> Int -- ^ The number of suppliers per order.
            -> Int -- ^ The number of features per supplier.
            -> Int -- ^ Minimal feature value.
            -> Int -- ^ Maximum feature value.
            -> [Int] -- ^ The feature to base picking the best supplier on.
            -> IO TrainingSet
generateSet' numOrders numSuppliers numFeatures fMin fMax pickFeatures = do
      examples <- mapM (\_ -> generateFeatures' numSuppliers numFeatures fMin fMax) 
                       [0..(numOrders - 1)]
      return $ zip examples (generatePicks examples pickFeatures)



-- | Generates a single random feature set.
generateFeatures :: Int -- ^ The number of suppliers to generate.
                 -> Int -- ^ The number of features per supplier.
                 -> IO FeatureSet
generateFeatures s f = generateFeatures' s f defaultFMin defaultFMax


-- | Generates a single random feature set.
generateFeatures' :: Int -- ^ The number of suppliers to generate.
                  -> Int -- ^ The number of features per supplier.
                  -> Int -- ^ Minimal feature value.
                  -> Int -- ^ Maximum feature value.
                  -> IO FeatureSet
generateFeatures' s f fMin fMax = do
      values <- evalRandIO $ replicateM (s * f) (getRandomR (fMin, fMax))
      return $ (s, f) <+> map fromIntegral values



-- | Generates a list of picked supplier indices, based on the highest value
--   in the given columns.
generatePicks :: [FeatureSet] -- ^ The features to compare.
              -> [Int]        -- ^ The feature column indices to base the picking on.
              -> [Int]
generatePicks fs [] = error "generatePicks: Column indices cannot be empty"
generatePicks fs cs = map pick fs
   where pick f = fromJust $ (maximum $ col f) `elemIndex` (col f)
         col f  = elements $ foldl (\m c -> m + (f !|! c)) (mInfinite 0) cs


----------------------------------------------------------------------------

-- | Changes the picked value in a given number of random training examples 
--   to a random value, where the new value /= original value.
insertErrors :: TrainingSet -- ^ The training set to modify.
             -> Int         -- ^ Number of errors to introduce.
             -> IO TrainingSet
insertErrors ts numErrors = do
   let numTExs = length ts
   indices     <- uniqueRandList 0 (numTExs - 1) numErrors
   foldM insertErrorAt ts indices
   


insertErrorAt :: TrainingSet -> Int -> IO TrainingSet
insertErrorAt ts index = do
   newTEx <- insertError (ts !! index)
   return $ take index ts ++ [newTEx] ++ drop (index + 1) ts


insertError :: TrainingExample -> IO TrainingExample
insertError (values, oldPicked) = do
   let newPickedOptions = [i | i <- [0..numRows values - 1], i /= oldPicked]
   newPicked <- randOneOf newPickedOptions
   return (values, newPicked)


----------------------------------------------------------------------------

-- | Picks a random element from the supplied list.
randOneOf :: [a] -> IO a
randOneOf xs = do
   index <- evalRandIO (getRandomR (0, length xs - 1))
   return $ xs !! index
   
-- | Generates a random integer in the supplied range. It ensures that the
--   generated number is not an element in the supplied exclude list.
randRangeExcept :: Int -> Int -> [Int] -> IO Int
randRangeExcept low high exclude = do
   n <- evalRandIO $ getRandomR (low, high)
   if n `elem` exclude then randRangeExcept low high exclude
                       else return n

-- | Generates a list of unique random numbers in the supplied range.
uniqueRandList :: Int -- ^ The lower range bound.
               -> Int -- ^ The upper range bound.
               -> Int -- ^ The number of numbers to generate.
               -> IO [Int]
uniqueRandList low high len | (len - 1) > (high - low) = error sizeErr
                            | otherwise                = nrs []
   where nrs acc | length acc < len = randRangeExcept low high acc >>= (\n -> nrs $ n : acc)
                 | otherwise        = return acc
         sizeErr = printf "uniqueRandList: Cannot create %d unique random numbers between [%d, %d]" len low high

                 