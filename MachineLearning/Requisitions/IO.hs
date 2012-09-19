module MachineLearning.Requisitions.IO (
   -- * Read
     trainingSetFromCSV
   , trainingSetFromCSV'
   -- * Write
   , trainingSetToCSV
   , trainingSetToCSV'
) where

import MachineLearning.Requisitions.Requisitions
import MachineLearning.Maths.Matrix
import Data.Char
import Data.Function
import Data.List
import Text.Regex
import Debug.Trace

----------------------------------------------------------------------------

sep = ";"

----------------------------------------------------------------------------

-- | Reads a set of training examples from a CSV string.
--
--   All lines will be grouped into matrices by their first value, REQ_ID.
--   At the moment the first occurrence of a new REQ_ID is assumed to be the 
--   picked supplier.
--
--   Expected input format:
--
-- > REQ_ID, SUPPLIER_ID, x1, x2, x3
-- > REQ_ID, SUPPLIER_ID, x1, x2, x3
trainingSetFromCSV :: String                     -- ^ CSV input string.
                   -> Bool                       -- ^ Flip polarities
                   -> (FeatureSet -> FeatureSet) -- ^ Transformation function.
                   -> IO (TrainingSet, Matrix String)
trainingSetFromCSV csv flipPolarities transformFn = do
    let ls          = lines csv                                  --break into lines.
    let filteredLs  = filter (not . isPrefixOf "#") $            --remove comment lines.
                      filter (not . null) $                      --remove empty lines.
                      map (filter (not . isSpace)) ls            --remove whitespace chars.
    let rows        = map (splitRegex (mkRegex sep)) filteredLs  --split at separator.
    let groups      = groupBy ((==) `on` head) rows              --group by requisition id.
    let groups'     = map (map (drop 2)) groups                  --remove requisition id and supplier id from lists.
    let ms          = map (M . map (map read)) groups'           --convert to matrices.
    let scaledMs    = map transformFn ms                         --scale feature matrices.

    let invertedFs  = getInvertedFs ls
    let msInverted  = if flipPolarities then (map (flipPolarity' invertedFs) scaledMs) 
                                        else scaledMs
    

    let ts          = map (\fs -> (fs, 0)) msInverted            --convert to training examples.
    let supplierIds = M $ map (map (head . tail)) groups         --Extract supplier id from groups.
    
    return (ts, supplierIds)


-- | Reads a set of training examples from a CSV file.
trainingSetFromCSV' :: FilePath                   -- ^ The csv file to read.
                    -> Bool                       -- ^ Flip polarities
                    -> (FeatureSet -> FeatureSet) -- ^ Transformation function.
                    -> IO (TrainingSet, Matrix String)
trainingSetFromCSV' csvFile flipPolarities transformFn = do
    csv <- readFile csvFile
    trainingSetFromCSV csv flipPolarities transformFn
    
    
invertFsToken = "#Invert:"
getInvertedFs :: [String] -> [Bool]
getInvertedFs ls = case (find (isPrefixOf invertFsToken) ls) of
                     Nothing             -> repeat False
                     (Just invertString) -> (map (== "T")
                                                 $ splitRegex (mkRegex sep)
                                                 $ filter (not . isSpace)
                                                 $ drop (length invertFsToken) invertString
                                            ) ++ repeat False

----------------------------------------------------------------------------


-- | Writes a training set to a comma separated values string.
--   Well..semicolon separated really...
trainingSetToCSV :: TrainingSet -> String
trainingSetToCSV ts = unlines 
                      $ zipWith (\ex req -> exampleToString ex ("REQ" ++ show req))
                                ts [1..]

-- | Writes a training set to a csv file.
trainingSetToCSV' :: FilePath -> TrainingSet -> IO ()
trainingSetToCSV' f ts = writeFile f $ trainingSetToCSV ts


exampleToString :: TrainingExample -> String -> String
exampleToString (fs, p) orderName = 
      unlines $ moveToFront p
              $ zipWith (\s xs -> (orderName ++ ";S" ++ show s ++ ";" ++ xs))
                        [1..]
                        (map (intercalate ";" . map show) (rows fs))
   
moveToFront :: Int -> [a] -> [a]
moveToFront i xs = (xs !! i) : take i xs ++ drop (i + 1) xs