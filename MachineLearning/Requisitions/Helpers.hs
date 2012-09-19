module MachineLearning.Requisitions.Helpers where
import MachineLearning.Requisitions.Random
import MachineLearning.Requisitions.Requisitions
import System.IO.Unsafe --FIX UNSAFEPERFORMIO!


---------------------------------------------------------------------------

-- | Accumulates error rates with the given algorithm configuration from 1
--   training step up to the given limit.
errorRates :: TrainingSet -> AlgorithmConfig -> Int -> [Double]
errorRates ts algorithm trainUpTo = map errorRateFn [1..trainUpTo]
   where rankFn        = getRankingFn algorithm
         trainFn       = getTrainingFn algorithm
         errorRateFn t = errorRate rankFn controlSet (trainFn $ trainSet t)
         trainSet t    = take t ts
         controlSet    = drop trainUpTo ts


---------------------------------------------------------------------------


sensitivity :: TrainingSet -> AlgorithmConfig -> Int -> Int -> [Double]
sensitivity ts algorithm trainUpTo errors = map errorRateFn [0..errors]
     where rankFn        = getRankingFn algorithm
           trainFn       = getTrainingFn algorithm
           errorRateFn t = errorRate rankFn controlSet (trainFn $ errorSet t)
           trainSet      = take trainUpTo ts
           errorSet t    = unsafePerformIO $ insertErrors trainSet t
           controlSet    = drop trainUpTo ts

