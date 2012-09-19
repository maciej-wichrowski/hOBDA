module Main where
import MachineLearning.Requisitions.Random
import MachineLearning.Requisitions.IO
import MachineLearning.Maths.Matrix
import Control.Monad
import Control.Monad.Random
import System.Environment
import Data.Maybe
import Debug.Trace


main :: IO ()
main = do
    args <- getArgs
    case args of
      (numOrd : numSup : numFeat : [])                        -> main' numOrd numSup numFeat (show 1) (show defaultFMin) (show defaultFMax)
      (numOrd : numSup : numFeat : pickFs : [])               -> main' numOrd numSup numFeat pickFs (show defaultFMin) (show defaultFMax)
      (numOrd : numSup : numFeat : pickFs : fMin : fMax : []) -> main' numOrd numSup numFeat pickFs fMin fMax
      _ -> error ("Argument count error.\n" ++ 
                  "Usage: Generate num_orders\n" ++
                  "                num_suppliers\n" ++
                  "                num_features\n" ++
                  "                [num_pick_features=1]\n" ++
                  "                [feature_min=0] [feature_max=100]")

main' numOrders numSuppliers numFeatures pickFs fMin fMax = do
      let numOrders'    = read numOrders
      let numSuppliers' = read numSuppliers
      let numFeatures'  = read numFeatures
      let pickFs'       = read pickFs
      let fMin'         = read fMin
      let fMax'         = read fMax
      pickFeatures      <- uniqueRandList 0 (numFeatures' - 1) pickFs'
      tSet              <- generateSet' numOrders' numSuppliers' numFeatures' fMin' fMax' pickFeatures
      putStr $ trainingSetToCSV tSet
