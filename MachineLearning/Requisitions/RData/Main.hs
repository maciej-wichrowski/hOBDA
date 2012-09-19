module Main where
import MachineLearning.Requisitions.Helpers
import MachineLearning.Requisitions.IO
import MachineLearning.Requisitions.Requisitions
import MachineLearning.Maths.Matrix
import System.Environment
import Debug.Trace


main :: IO ()
main = do 
   args <- getArgs
   case args of
      ("errorRates"  : tSetFile : (alg:[]) : [])    -> printErrorRates tSetFile (getAlgorithmConfig alg)
      ("sensitivity" : tSetFile : (alg:[]) : [])    -> printSensitivity tSetFile (getAlgorithmConfig alg)
      ("featureWeights" : tSetFile : (alg:[]) : []) -> printFeatureWeights tSetFile (getAlgorithmConfig alg)
      ("featureGroupSize" : (alg:[]) : [])          -> printFeatureGroupSize (getAlgorithmConfig alg)
      _                                             -> usageError


usageError = error ("Argument error.\n"
                    ++ "Usage: \t\t RData function [options]\n"
                    ++ "Functions:"
                    ++ "\t errorRates csv_file algorithm(A-C)\n"
                    ++ "\t\t sensitivity csv_file algorithm(A-C)\n"
                    ++ "\t\t featureWeights csv_file algorithm(A-C)\n"
                    ++ "\t\t featureGroupSize algorithm(A-C)")


----------------------------------------------------------------------------

listToCSV :: (Show a) => [String] -> [a] -> String
listToCSV xs ys = "X;Y\n" ++ unlines (zipWith (\x y -> x ++ ";" ++ show y) xs ys)

listToCSV' :: (Show a) => [a] -> String
listToCSV' = listToCSV (map show [1..])

transformFn :: AlgorithmConfig -> FeatureSet -> FeatureSet
transformFn algConfig = getPostScaleFn algConfig 
                        . scaleFeatures 
                        . getPreScaleFn algConfig

----------------------------------------------------------------------------


printErrorRates :: FilePath -> AlgorithmConfig -> IO ()
printErrorRates csvFile algConfig = do
   (tSet, _)      <- trainingSetFromCSV' csvFile (getFlipPolarities algConfig) (transformFn algConfig)
   let trainUpTo  = length tSet `div` 2
   let rates      = errorRates tSet algConfig trainUpTo
   putStr $ listToCSV' rates


----------------------------------------------------------------------------


printSensitivity :: FilePath -> AlgorithmConfig -> IO ()
printSensitivity csvFile algConfig = do
   (tSet, _)      <- trainingSetFromCSV' csvFile (getFlipPolarities algConfig) (transformFn algConfig)
   let trainUpTo  = length tSet `div` 2
   let rates      = sensitivity tSet algConfig trainUpTo trainUpTo
   putStr $ listToCSV' rates


----------------------------------------------------------------------------


printFeatureWeights :: FilePath -> AlgorithmConfig -> IO ()
printFeatureWeights csvFile algConfig = do
   (tSet, _)      <- trainingSetFromCSV' csvFile (getFlipPolarities algConfig) (transformFn algConfig)
   let trainUpTo  = length tSet `div` 2
   let weights    = (getTrainingFn algConfig) (take trainUpTo tSet)
   putStr $ listToCSV (featureLabels $ getFeatureGroupSize algConfig) 
                      (elements weights)
   

----------------------------------------------------------------------------


printFeatureGroupSize :: AlgorithmConfig -> IO ()
printFeatureGroupSize algConfig = putStrLn $ listToCSV'
                                           $ [show $ getFeatureGroupSize algConfig]

