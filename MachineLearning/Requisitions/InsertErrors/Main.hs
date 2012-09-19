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
      (csvFile : numErrors : []) -> main' csvFile (read numErrors)
      _ -> error ("Argument count error.\n" ++ 
                  "Usage: InsertErrors original_set_csv\n" ++
                  "                    num_errors\n")

main' csvFile numErrors = do
      (ts, _)        <- trainingSetFromCSV' csvFile False id
      let trainUpTo  = length ts `div` 2
      let trainSet   = take trainUpTo ts
      let controlSet = drop trainUpTo ts
      errorSet       <- insertErrors trainSet numErrors
      putStr $ trainingSetToCSV (errorSet ++ controlSet)
