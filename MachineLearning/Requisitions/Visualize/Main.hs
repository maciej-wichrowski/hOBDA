module Main where
import MachineLearning.Requisitions.Helpers
import MachineLearning.Requisitions.Requisitions
import MachineLearning.Requisitions.IO
import MachineLearning.Maths.Matrix
import MachineLearning.Maths.Statistics
import MachineLearning.Helpers
import Graphics.HsCharts
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Environment
import Debug.Trace

----------------------------------------------------------------------------

--Window properties
wWidth  = 1000
wHeight = 700
wTitle  = "Requisitions"
chartW = 180
chartH = 160
chartP = 50

----------------------------------------------------------------------------

data SimWorld = SimWorld { getTrainingSet :: TrainingSet
                         , getTrainingSetUnscaled :: TrainingSet
                         , getSupplierNames :: Matrix String
                         , getAlgConfig :: AlgorithmConfig
                         , getCurrentTEx :: Int
                         , getShowRows :: [Bool]
                         , getStopTrainingAt :: Int
                         }

----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        (tSetFile : (alg:[]) : []) -> main' tSetFile alg
        _                          -> error "Usage: Visualize csv-file algorithm(A-C)"


main' :: FilePath -> Char -> IO()
main' tFile algorithmChar = do
    let algConfig      = getAlgorithmConfig algorithmChar
    let preScaleFn     = getPreScaleFn algConfig
    let postScaleFn    = getPostScaleFn algConfig
    let flipPolarities = getFlipPolarities algConfig
    (unscaledTSet, supNames) <- trainingSetFromCSV' tFile False id
    (trainingSet, _)         <- trainingSetFromCSV' tFile flipPolarities (postScaleFn . scaleFeatures . preScaleFn)
    let disp                 = InWindow wTitle (wWidth, wHeight) (50, 50) 
    let trainStop            = (length trainingSet) `div` 2
    play disp white 1
         (SimWorld trainingSet unscaledTSet supNames algConfig 0 [True, False, False] trainStop)
         plotWorld
         handleInput
         stepWorld


-----------------------------------------------------------------------------


handleInput :: Event -> SimWorld -> SimWorld
handleInput (EventKey (SpecialKey KeyRight) Down _ _) w = nextTExample w
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) w = prevTExample w
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) w = lastTExample w
handleInput (EventKey (SpecialKey KeyDown)  Down _ _) w = firstTExample w
handleInput (EventKey (Char c) Down _ _) w | c == '0'             = showAllRows w
                                           | c >= '1' && c <= '9' = showRow (digitToInt c - 1) w
                                           | otherwise            = w
handleInput _ w = w

stepWorld :: Float -> SimWorld -> SimWorld
stepWorld _ w = w

nextTExample :: SimWorld -> SimWorld
nextTExample w = w{ getCurrentTEx = minimum [ getCurrentTEx w + 1
                                            , length (getTrainingSet w) - 1] }

prevTExample :: SimWorld -> SimWorld
prevTExample w = w{ getCurrentTEx = maximum [ getCurrentTEx w - 1, 0] }

firstTExample :: SimWorld -> SimWorld
firstTExample w = w{ getCurrentTEx = 0 }

lastTExample :: SimWorld -> SimWorld
lastTExample w = w{ getCurrentTEx = length (getTrainingSet w) - 1 }

showRow :: Int -> SimWorld -> SimWorld
showRow i w = w{ getShowRows = take i s ++ [not (s !! i)] ++ drop (i + 1) s }
    where s = getShowRows w

showAllRows :: SimWorld -> SimWorld
showAllRows w = w{ getShowRows = replicate 3 (not $ all id (getShowRows w)) }


-----------------------------------------------------------------------------

plotWorld :: SimWorld -> Picture
plotWorld w = pictures $ row1 (head $ getShowRows w)
                         ++ row2 (getShowRows w !! 1)
                         ++ row3 (getShowRows w !! 2)
    where
        row1 True = [ r0 $ scaleText $ text ("Training Example # " ++ show (currentTExample + 1) ++ "/" ++ show (length trainingSet))
                    , r1 $ translate (-150) 125 $ drawFeatureTable tExampleUnscaled tExampleNames tExampleScores symbolList
                    , r1 $ translate (-(chartW * 2 + chartM * 3)) r2ChartY $ plotTrainingExample tExampleFeatures algConfig symbolList tExampleNames
                    , r1 $ translate (-(chartW * 2 + chartM * 3)) (rowHeight - 20) $ scaleText $ text "Scaled Features"
                    ]
        row1 _    = [] 
        row2 True = [ r1 hLine
                    , r2 $ translate (-(chartW * 2 + chartM * 3)) r2ChartY $ plotFeatureWeights tExampleWeights algConfig
                    , r2 $ translate (-(chartW * 2 + chartM * 3)) (rowHeight - 20) $ scaleText $ text "Feature Weights"
                    , r2 $ translate (-(chartW + chartM))         r2ChartY $ plotFeatureWeights weightsPts algConfig
                    , r2 $ translate (-(chartW + chartM))         (rowHeight - 20) $ scaleText $ text "Cumulative Feature Weights"
                    , r2 $ translate chartM                       r2ChartY $ plotFeatureWeights finalWeightsPts algConfig
                    , r2 $ translate chartM                       (rowHeight - 20) $ scaleText $ text "Final Feature Weights"
                    , r2 $ translate (chartW + chartM * 3)        r2ChartY $ plotErrorRates errRatePts
                    , r2 $ translate (chartW + chartM * 3)        (rowHeight - 20) $ scaleText $ text "Error Rate"
                    ]
        row2 _    = []
        row3 True = [ r2 hLine
                    , r3 $ translate ((-fromIntegral wWidth / 2) + 25) 0 $ plotTrends trainingSet algConfig (getSupplierNames w) symbolList (getStopTrainingAt w)
                    , r3 $ translate ((-fromIntegral wWidth / 2) + 25) (chartH + 20) $ scaleText $ text "Overall trend"
                    ]
        row3 _    = []
        trainingSet      = getTrainingSet w
        currentTExample  = getCurrentTEx w
        algConfig        = getAlgConfig w
        trainUpTo        = getStopTrainingAt w
        weightsFn        = getTrainingFn algConfig
        rankFn           = getRankingFn algConfig
        supplierNames    = nub $ elements $ getSupplierNames w
        symbolList       = supplierSymbolMap supplierNames
        tExampleNames    = elements $ getSupplierNames w !-! currentTExample
        tExampleUnscaled = getTrainingSetUnscaled w !! currentTExample
        tExampleFeatures = trainingSet !! currentTExample
        tExampleWeights  = zip [1..] (map double2Float $ elements $ weightsFn [tExampleFeatures])
        trainUpToEx	    = min trainUpTo (currentTExample + 1)
        weightsUpToNow   = weightsFn $ take trainUpToEx trainingSet
        weightsPts       = zip [1..] (map double2Float $ elements $ adjustCombinedWeights w $ weightsUpToNow)
        finalWeightsPts  = zip [1..] (map double2Float $ elements $ adjustCombinedWeights w $ weightsFn $ take trainUpTo trainingSet)
        tExampleScores   = rankFn (fst tExampleFeatures) weightsUpToNow
        errRatePts       = zip [1..] $ map double2Float $ errorRates trainingSet algConfig trainUpTo
        wWidthHalf       = fromIntegral wWidth / 2
        hLine            = line [(-wWidthHalf, 0), (wWidthHalf, 0)]
        rowHeight        = fromIntegral wHeight / 3
        chartM           = (fromIntegral wWidth - 4 * chartW) / 8
        r0               = translate (-50) ((fromIntegral wHeight / 2) - 50)
        r1               = translate 0 (0.5 * rowHeight)
        r2               = translate 0 ((-0.5) * rowHeight)
        r3               = translate 0 (-(fromIntegral wHeight / 2) + 30)
        r2ChartY         = (rowHeight - chartH) / 2
        

----------------------------------------------------------------------------

adjustCombinedWeights :: SimWorld -> FeatureWeights -> FeatureWeights
adjustCombinedWeights s fs | doTransform = M $ [zipWith (-) fs' mins]
                           | otherwise   = fs
   where groupSize         = round $ getFeatureGroupSize $ getAlgConfig s
         doTransform       = groupSize > 1
         fs'               = elements fs
         mins              = replicate' groupSize $ zipWith min (skip groupSize fs') (skip groupSize $ drop 1 fs')
         combinedValue a b = min a b
   
skip _ [] = []
skip n xs = (head xs) : skip n (drop n xs)

replicate' n xs = concatMap (replicate n) xs

----------------------------------------------------------------------------

plotTrends :: TrainingSet -> AlgorithmConfig -> Matrix String -> SymbolMap -> Int -> Picture
plotTrends ts algConfig ns symbols stopTrain = pictures 
       [ bgColor $ plotChartBackground xAxis yAxis
       , gridColor $ plotGrid xAxis yAxis (0, 2)
       , plotAxes xAxis yAxis
       , plotAxisScales xAxis yAxis (1, 2)
       , pickedArea $ elements (scores !-! 0)
       , pictures $ map line scoreGroups
       , pictures $ map point scoreGroups
       ]
    where
        weightsFn            = getTrainingFn algConfig
        rankFn               = getRankingFn algConfig
        line r               = color (supplierColor symbols (fst $ head r)) $ plotLineChart xAxis yAxis (zip [1..] (map snd r))
        point r              = plotPointChart' (supplierSymbol symbols (fst $ head r) False) xAxis yAxis (zip [1..] (map snd r))
        pickedArea r         = color (makeColor 0.5 0.5 0.5 0.1) $ plotAreaChart xAxis yAxis (zip [1..] r)
        scores               = mMap double2Float $ foldl1 (+|+) $ map ranks (drop 1 $ inits ts)
        scoresAndNames       = mZip (\a b -> (a, b)) (mTranspose ns) scores
        sortedScoresAndNames = sortBy (compare `on` fst) $ concat $ cols scoresAndNames
        scoreGroups          = groupBy ((==) `on` fst) sortedScoresAndNames
        weights              = weightsFn $ take stopTrain ts
        ranks ts'            = rankFn (fst $ last ts') weights
        xAxis                = fixedScaleAxis Linear (fromIntegral wWidth - 50) 1 (fromIntegral (numCols scores))
        yAxis                = autoScaleAxis Linear chartH (elements scores)
        
        

----------------------------------------------------------------------------

scaleText     = scale 0.10 0.10

bgColor       = color (makeColor 0.98 0.98 0.98 1)

gridColor     = color (makeColor 0.8 0.8 0.8 1)
gridAltColor  = color (makeColor 0.8 0.8 0.8 0.4)

pointColor    = color (makeColor 0.15 0.50 0.75 1)
barColor      = color (makeColor 0.15 0.50 0.75 0.8)
areaColor     = color (makeColor 0.15 0.50 0.75 0.4)

pickedColor   = color (makeColor 0.2 0.9 0.2 1)

meanColor     = color (makeColor 0 0 1 0.3)
meanPicture   = meanColor $ line [(-10, 0), (10, 0)]

type SymbolMap = [(String, (Picture, Color))]

supplierSymbolSize = 6
supplierSymbols = cycle [ cross supplierSymbolSize
                        , eqTriangleSolid supplierSymbolSize
                        , rectangleSolid supplierSymbolSize supplierSymbolSize
                        , circle (supplierSymbolSize / 2)
                        , diamondSolid supplierSymbolSize 0.8
                        ]
                        
supplierColors = cycle [ violet, orange, azure, red, cyan, rose, 
                         blue, chartreuse, aquamarine, magenta
                       ]

supplierSymbolMap :: [String] -> SymbolMap
supplierSymbolMap sups = zip sups $ zip supplierSymbols supplierColors

supplierSymbol :: SymbolMap -> String -> Bool -> Picture
supplierSymbol sups s True = pickedColor $ pictures [ circle 5
                                                    , maybe blank fst (lookup s sups)
                                                    ]
supplierSymbol sups s _    = maybe blank fst (lookup s sups)

supplierColor sups s = maybe black snd (lookup s sups)

                             

----------------------------------------------------------------------------

drawFeatureTable :: TrainingExample -> [String] -> Matrix Double -> SymbolMap -> Picture
drawFeatureTable ts ns scores symbols = 
    pictures $    supplierSymbolCol
               ++ zipWith drawName ns [1..]
               ++ concat (zipWith drawRow (rows $ fst ts) [1..])
               ++ [drawScores]
               ++ zipWith drawScore (elements scores) [1..]
               ++ drawLabels
               ++ [drawHLine]
               ++ drawVLines
    where dataY       = -5
          symbolColW  = 20
          nameColW    = 100
          featureColW = 60
          scoreColX   = nameColW + nCols * featureColW
          scoreColW   = 150
          rowHeight   = 20
          tableW      = symbolColW + nameColW + featureColW * nCols + scoreColW
          nRows       = fromIntegral $ numRows (fst ts)
          nCols       = fromIntegral $ numCols (fst ts)
          supplierSymbolCol = [ translate (-symbolColW) (5 + (s * (-20))) (supplierSymbol symbols (ns !! (round s - 1)) (s == 1.0)) 
                              | s <- [1.0 .. nRows]]
          drawName n i  = translate 0 (i * (-20)) (scaleText $ text n)
          drawRow r i   = map (translate nameColW (i * (-20))) (zipWith drawCol r [0..])
          drawCol c i   = translate (i * featureColW) 0 (scaleText $ text (showFDouble' 1 c))
          drawScores    = translate scoreColX dataY $
                          rotate 90 $ 
                          barColor $ plotBarChart xAxis yAxis $ zip [1..] (map double2Float (elements scores))
          xAxis         = fixedScaleAxis Linear (nRows * rowHeight) 0 nRows
          yAxis         = autoScaleAxis' Linear scoreColW (map double2Float (elements scores))
          drawScore s i = translate tableW (i * (-20)) (scaleText $ text (showFDouble' 2 s))
          
          drawLabels =    [scaleText $ text "Supplier"] 
                       ++ zipWith drawFeatureLbl (take (numCols (fst ts)) ['p'..'z']) [0..]
                       ++ [translate scoreColX 0 $ scaleText $ text "Score"]
          drawFeatureLbl f i = translate (nameColW + i * featureColW) 0 $ scaleText (text [f])
          drawHLine  = line [(-symbolColW * 1.5, dataY), (tableW - symbolColW + 75, dataY)]
          drawVLines = [ vLine 0, vLine nameColW ] 
                       ++ map (\f -> vLine (nameColW + f * featureColW)) [0..nCols]
          vLine x = line [(-5 + x, 13), (-5 + x, 13 - rowHeight * (nRows + 1))]
          

----------------------------------------------------------------------------


--Plots a training example (matrix, int) where the picked row is rendered 
--differently from the others. Also renders the mean values of the columns.
plotTrainingExample :: TrainingExample -> AlgorithmConfig -> SymbolMap -> [String] -> Picture
plotTrainingExample (fs, p) algConfig symbols ns =
    pictures [ bgColor $ plotChartBackground xAxis yAxis
             , gridColor $ plotGrid xAxis yAxis (1, 0)
             , plotGrid xAxis yAxis (groupSize, 0)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (0, 0.5)
             , plotAxisScaleLabels xAxis yAxis xLabels
             , pictures (plotRows fs 0.0)
             , plotPointChart' meanPicture xAxis yAxis meanPts
             ]
    where 
        groupSize    = getFeatureGroupSize algConfig
        numFeatures  = fromIntegral $ numCols fs
        xAxis        = fixedScaleAxis Linear (chartW + 50) 0 numFeatures
        yAxis        = fixedScaleAxis Linear chartH 0 1
        pickedPts    = matrixToPoints $ fs !-! p
        meanPts      = matrixToPoints $ cMean fs
        xLabels      = zipWith (\(x,y) f -> (x, f)) pickedPts $ featureLabels groupSize
        plotRows m i | isEmpty m = []
                     | otherwise = plotSupplier (takeR 1 m) (supplierSymbol symbols (ns !! round i) (i == 0))
                                   : plotRows (dropR 1 m) (i + 1)
        plotSupplier r p = plotPointChart' p xAxis yAxis (matrixToPoints r)
        


matrixToPoints :: Matrix Double -> [Point]
matrixToPoints m = concatMap (zip [0.5..] . map double2Float) (rows m)

----------------------------------------------------------------------------

plotFeatureWeights :: [Point] -> AlgorithmConfig -> Picture
plotFeatureWeights pts algConfig = 
         pictures [ bgColor $ plotChartBackground xAxis yAxis
                --, gridAltColor $ plotGrid xAxis yAxis (0, 0.1)
                  , gridColor $ plotGrid xAxis yAxis (0, 0.2)
                  , plotGrid xAxis yAxis (groupSize, 0)
                  , plotAxisX xAxis yAxis
                  , plotAxisScales xAxis yAxis (0, 0.2)
                  , plotAxisScaleLabels xAxis yAxis xLabels
                  , barColor $ plotBarChart xAxis yAxis pts
                  ]
    where (xs, ys)  = unzip pts
          xAxis     = autoScaleAxis' Linear chartW xs
          yAxis     = autoScaleAxis' Linear chartH ys
          xLabels   = zipWith (\(x,y) f -> (x - 0.5, f)) pts $ featureLabels groupSize
          groupSize = getFeatureGroupSize algConfig

----------------------------------------------------------------------------

plotErrorRates :: [Point] -> Picture
plotErrorRates pts = pictures [ bgColor $ plotChartBackground xAxis yAxis
                              , gridColor $ plotGrid xAxis yAxis (0, 0.1)
                              , plotAxes xAxis yAxis
                              , plotAxisScales xAxis yAxis (1, 0.1)
                              , areaColor $ plotAreaChart xAxis yAxis pts
                              , pointColor $ plotLineChart xAxis yAxis pts
                              , pointColor $ plotPointChart' (circleSolid 2) xAxis yAxis pts
                              ]
    where (xs, ys) = unzip pts
          xAxis    = autoScaleAxis Linear chartW xs
          yAxis    = fixedScaleAxis Linear chartH 0 1 --autoScaleAxis Linear chartH ys

----------------------------------------------------------------------------

