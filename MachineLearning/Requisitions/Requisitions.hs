module MachineLearning.Requisitions.Requisitions (
    -- * Type Synonyms
    FeatureSet,
    FeatureWeights,
    TrainingExample,
    TrainingSet,
    RankFunction,
    -- * Configurations
    AlgorithmConfig(..),
    algorithmConfigs,
    getAlgorithmConfig,
    featureLabels,
    -- * Functions
    scaleFeatures,
    featureWeightsDAvg,
    featureWeightsSum,
    rank,
    pickBest,
    errorRate,
    flipPolarity,
    flipPolarity',
    addPolarizedFeatures,
    addPolarizedFeatures',
    rank',
    rank'',
) where

import MachineLearning.Helpers
import MachineLearning.Maths.Matrix
import MachineLearning.Maths.Statistics
import Data.List
import Debug.Trace

-----------------------------------------------------------------------------

-- | A block-matrix with supplier feature values (features across columns).
type FeatureSet      = Matrix Double
-- | A row-vector of feature weights.
type FeatureWeights  = Matrix Double
-- | A combination of a feature matrix and picked supplier index.
type TrainingExample = (FeatureSet, Int)
type TrainingSet     = [TrainingExample]
type RankFunction    = FeatureSet        -- ^ Set of supplier features (matrix).
                       -> FeatureWeights -- ^ Feature weights (row vector).
                       -> Matrix Double  -- ^ Supplier scores (column vector).


-----------------------------------------------------------------------------


data AlgorithmConfig = AlgorithmConfig { getTrainingFn       :: TrainingSet -> FeatureWeights
                                       , getRankingFn        :: RankFunction
                                       , getPreScaleFn       :: FeatureSet -> FeatureSet
                                       , getPostScaleFn      :: FeatureSet -> FeatureSet
                                       , getFlipPolarities   :: Bool
                                       , getFeatureGroupSize :: Float
                                       }


algorithmConfigs :: [(Char, AlgorithmConfig)]
algorithmConfigs = 
      [ 
         --A. Sum of feature values, single features
        ('A', (AlgorithmConfig featureWeightsSum rank id id True 1))
         --B. Distance to average, single features
      , ('B', (AlgorithmConfig featureWeightsDAvg rank id id True 1))
         --C. Summed weights with polarized (p-, p+) features. Use only highest difference of weights.
      , ('C', (AlgorithmConfig featureWeightsSum rank'' id addPolarizedFeatures' False 2))
         --D? Summed weights with polarized (p-, pL, p+) features
      --, ('D', (AlgorithmConfig featureWeightsSum rank id addPolarizedFeatures 3))
         --E? Summed weights with polarized (p-, pL, p+) features. Use only highest weights.
      --, ('E', (AlgorithmConfig featureWeightsSum rank' id addPolarizedFeatures 3))
         --F? Summed weights with polarized (p-, p+) features
      --, ('F', (AlgorithmConfig featureWeightsSum rank id addPolarizedFeatures' 3))
      ]
       
getAlgorithmConfig :: Char -> AlgorithmConfig
getAlgorithmConfig a = case (lookup a algorithmConfigs) of
                        (Just alg) -> alg
                        Nothing    -> error $ "Failed to lookup algorithm configuration " ++ [a]

featureLabels :: Float -> [String]
featureLabels 2 = zipWith (:) (concatMap (replicate 2) ['p'..'z']) 
                              (cycle ["-", "+"])
featureLabels 3 = zipWith (:) (concatMap (replicate 3) ['p'..'z']) 
                              (cycle ["-", "L", "+"])
featureLabels _ = map (:[]) ['p'..'z']


-----------------------------------------------------------------------------


cMin = cMap (repeat . minimum)
cMax = cMap (repeat . maximum)

-- | Scales the columns of the given matrix such that the values are 
--   all within the range \<0, 1\>.
scaleFeatures :: FeatureSet -> FeatureSet
scaleFeatures m = mMap replaceNaN ((m - mins) / range)
    where mins  = cMin m
          maxs  = cMax m
          range = maxs - mins

replaceNaN n | isNaN n   = 0.5
             | otherwise = n

-- | Calculates the difference between the 'picked' row in the matrix and the
--   mean values of all rows in the matrix.
distanceToAvg :: TrainingExample -> FeatureWeights
distanceToAvg (m, p) = (m !-! p) - cMean m

-- | Maps distanceToAvg function over all training examples and sums the results.
featureWeightsDAvg :: TrainingSet -> FeatureWeights
featureWeightsDAvg [] = M [repeat 0]
featureWeightsDAvg ts = foldl1 (+) (map distanceToAvg ts) 

				--	mMap isZero weights
				--	where
				--	weights =  foldl1 (+) (map distanceToAvg ts) !/ fromIntegral (length ts)
				--IGNORE HLINT sum suggestion here!
-- foldl1 (+) (map ((mMap abs) . distanceToAvg) ts) !/ fromIntegral (length ts)

				--

isZero :: Double -> Double
isZero x | x < 0  = 0 
		 | otherwise =x
-----------------------------------------------------------------------------

-- | Alternative method of getting feature weights. Instead of distance to 
--   average feature value, it uses the absolute feature values as the weights.
featureWeightsSum :: TrainingSet -> FeatureWeights
featureWeightsSum [] = M [repeat 0]
featureWeightsSum ts = cSum $ foldl1 (+-+) (map (uncurry (!-!)) ts)

-----------------------------------------------------------------------------

-- | Ranks the supplier's feature set, returning a column-vector of scores.
--
-- > r = features * (weights T)
rank :: RankFunction
rank fs ws = fs `mMul` mTranspose ws

-- | Returns the index of the highest-ranked supplier.
pickBest :: RankFunction -> FeatureSet -> FeatureWeights -> Int
pickBest rankFn fs ws = case elemIndex max (head (cols ranks)) of
                            (Just i) -> i
                            Nothing  -> error "Could not find highest ranked supplier..."
    where ranks = rankFn fs ws
          max   = mFirst $ cMax ranks
          

-- | Calculates the errorrate of the supplied weights when using them on a 
--   training set. Lower value is better.
errorRate :: RankFunction -> TrainingSet -> FeatureWeights -> Double
errorRate rankFn ts ws = fromIntegral numCorrect / fromIntegral (length ts)
    where pickedByAlg = map (\s -> pickBest rankFn (fst s) ws) ts
          numCorrect  = length $ filter (uncurry (/=)) 
                                        (zip (map snd ts) pickedByAlg)


-----------------------------------------------------------------------------

-- | Flip the polarity of the entire feature set (negating all values)
flipPolarity :: FeatureSet -> FeatureSet
flipPolarity ts = mZip (+) ( mMap negate ts) (repeatR $ cMap (\r -> [maximum r]) ts )


-- | Flips the polarity of the columns in the supplied boolean list.
flipPolarity' :: [Bool] -> FeatureSet -> FeatureSet
flipPolarity' invs ts = mTranspose $ M $ zipWith invert invs (cols ts)
   where invert False xs = xs
         invert True  xs = zipWith (+) (map (negate) xs) (repeat $ (maximum xs + minimum xs))

-----------------------------------------------------------------------------

-- | Add polarized features to feature set.
--   p -> p-, pAverage, p+
addPolarizedFeatures :: FeatureSet -> FeatureSet
addPolarizedFeatures fs = mTranspose
                          $ M
                          $ foldl1 (++)
                          $ zipWith3 (\a b c -> [a, b, c]) 
                                     (cols lowFs) 
                                     (cols lagomFs) 
                                     (cols highFs)
    where lowFs   = mMap (1 -) fs
          lagomFs = mMap (1 -) $ mMap abs $ fs - (M $ repeat $ elements $ cMean fs)
          highFs  = fs


-- | Add polarized features to feature set.
--   p -> p-, p+
addPolarizedFeatures' :: FeatureSet -> FeatureSet
addPolarizedFeatures' fs = mTranspose
                          $ M
                          $ foldl1 (++)
                          $ zipWith (\a b -> [a, b]) 
                                    (cols lowFs) 
                                    (cols highFs)
    where lowFs   = mMap (1 -) fs
          highFs  = fs

-----------------------------------------------------------------------------


-- | Rank using only highest weight of the polarized features.
rank' :: RankFunction
rank' fs ws = rank selFeatures selWeights
    where 
        fChunks     = chunksC' fs 3
        wChunks     = chunksC' ws 3
        selFeatures = foldl1 (+|+) $ zipWith selCol fChunks wChunks
        selWeights  = foldl1 (+|+) $ zipWith selCol wChunks wChunks
        selCol f w  = fSource (w !!! (0, 0)) (w !!! (0, 1)) (w !!! (0, 2)) f
        fSource wLow wLag wHigh cs | wLow > wLag && wLow > wHigh = cs !|! 0
                                   | wLag > wHigh                = cs !|! 1
                                   | otherwise                   = cs !|! 2


-- ONLY USE WHEN USING 2 POLARITY FEATURES!
-- | Rank using only highest weight of the + and - polarities.
--   The resulting weight used is the (absolute) difference between + and -.
rank'' :: RankFunction
rank'' fs ws = rank selFeatures selWeights
    where 
        fChunks     = chunksC' fs 2
        wChunks     = map (\cs -> cs - cReverse cs) $ chunksC' ws 2
        selFeatures = foldl1 (+|+) $ zipWith selCol fChunks wChunks
        selWeights  = foldl1 (+|+) $ zipWith selCol wChunks wChunks
        selCol f w  = fSource (w !!! (0, 0)) (w !!! (0, 1)) f
        fSource wLow wHigh cs | wLow > wHigh = cs !|! 0
                              | otherwise    = cs !|! 1
                              
cReverse m = M $ map reverse $ rows m
-----------------------------------------------------------------------------

