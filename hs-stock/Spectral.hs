-- --------------------------------------------------------------------
-- | Spectral Module
-- --------------------------------------------------------------------
-- |    Module containing spectral clustering algorithm.
-- --------------------------------------------------------------------

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Spectral where
import Matrix
import KMeans
import Data.List
import System.Random
import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BSL
import qualified Graphics.EasyPlot as GE

-- --------------------------------------------------------------------
-- | Core Spectral Clustering Functions
-- --------------------------------------------------------------------
-- |

-- | degree:
-- |    Calc the Degree (D) matrix 
-- |    (from the Weighted Adjacency (W) matrix)
-- |    (for each row, sum all elements of row and place result in diagonal) 
-- |    (all zeros elsewhere)
-- --------------------------------------------------------------------
-- |    m = input matrix to be transformed into its degree matrix
degree :: Matrix -> Matrix
degree m 
    -- first create a identity matrix from the original 
    -- then calculate the rows sums 
    -- then multiply the vector by the ident matrix 
    = let idM = identM m
          ds  = [vectorSum x | x <- m] 
        in idM ||*| ds
          
-- | laplac:
-- |    Calc the Laplacian (L) Matrix
-- |    scale w matrix by -1, and add to d matrix
-- |    (unnormalised laplacian)
-- --------------------------------------------------------------------
-- |    d = degree matrix 
-- |    w = weight adjacency matrix
laplac :: Matrix -> Matrix -> Matrix
laplac d w = d ||+|| ((-1) `msdot` w)


-- | computeE:
-- |    Calc the Eigenvalues and Eigenvectors (output being a list of 
-- |    pairs of eigenvalues and vector).
-- --------------------------------------------------------------------
-- |    m = input matrix 
-- |    k = number of eigenvectors to be choosen (corresponding to k
-- |        smallest eigenvalues)
computeE :: Matrix -> Int -> [(Double, [Double])] -- Matrix
computeE m k 
    = let (evals, evecs) = eig m
          e = filter trimNeg $ zipWith (,) evals evecs
          e' = take k $ sortBy cmpEv e
          in
       e' ++ replicate ((length m)-k) (0, replicate (length m) 0)
          where
            cmpEv:: (Double, [Double]) -> (Double, [Double]) -> Ordering
            cmpEv (e1, _) (e2, _) = compare e1 e2

            trimNeg :: (Double, [Double]) -> Bool
            trimNeg (e, _) | e > 0 = True
                            | otherwise = False

-- --------------------------------------------------------------------
-- | Top level Spectral Clustering algorithm
-- --------------------------------------------------------------------
-- |

-- --------------------------------------------------------------------
spectral :: Int -> Double -> [(String, (Double, Double))] -> IO ()
spectral k sig pnts = do
        let 
            -- convert data set to weighted adjacency matrix (using simFn)
            w = fromList pnts sig
            -- calc the degree matrix
            d = degree w  
            -- calc the laplacian matrix
            l = laplac d w
            -- calc the eigen decomposition, eigenvectors are columns of evecs
            (_, evecs) = unzip $ computeE l k
            -- form a new matrix e with the eigenvectors as rows, and
            -- restore information from the original matrix when forming this new matrix
            -- (this is essentially connecting the graph based off the eigenvectors)
            e = restoreM $ updM l (transpose evecs)
        -- perform K-means clustering
        cls <- kmeans k e
        -- output results
        let
            f1 = "results.csv" 
            f2 = "original-data-plot.png"
            f3 = "clustered-data-plot.png"
        BSL.writeFile f1 $ CSV.encode $ mconcat $ map (\x -> x ++ [[]]) cls
        _ <- GE.plot (GE.PNG f2)  $ listPlot' pnts
        _ <- GE.plot (GE.PNG f3) $ map listPlot (cls)
        putStrLn $ "\nPrinting results to: \n" ++ f1 ++ "\n" ++ f2 ++ "\n" ++ f3 ++ "\n"
           where
              listPlot = GE.Data2D [GE.Title "", GE.Style GE.Dots] [] . getScoreL
              listPlot' = GE.Data2D [GE.Title "", GE.Style GE.Dots] [] . map (\(_,y) -> y)


spectralRand :: IO () -- GHC.Conc.Sync.ThreadId
spectralRand = do
        let n = 400
            dim = 2
            k = 4
            sig = 0.5
        seed  <- newStdGen
        datum <- sequence [ mkCluster (n `div` k) dim 0.2 (1.00) 
                          , mkCluster (n `div` k) dim 0.2 3.00
                          , mkCluster (n `div` k) dim 0.2 5.00 
                          , mkCluster (n `div` k) dim 0.2 0.00 ]
        let r = (mconcat datum)::[[Double]]
            alp = "abcdefghijklmnopqrstuvwxyz" 
            intL = randomL (n) seed
            namesL = [ replicate 3 x 
                     | x <- map (\x -> alp !! (x `mod` 26)) intL]
            pnts = (mkScoreL namesL r)
            -- convert data set to weighted adjacency matrix (using simFn)
            w = fromList pnts sig
            -- calc the degree matrix
            d = degree w  
            -- calc the laplacian matrix
            l = laplac d w
            -- calc the eigen decomposition, eigenvectors are columns of evecs
            (_, evecs) = unzip $ computeE l k
            -- form a new matrix e with the eigenvectors as rows, and
            -- restore information from the original matrix when forming this new matrix
            -- (this is essentially connecting the graph based off the eigenvectors)
            e = restoreM $ updM l (transpose evecs)
        -- perform K-means clustering
        cls <- kmeans k e
        -- output results
        let
            f1 = "rand-results.csv" 
            f2 = "rand-original-data-plot.png"
            f3 = "rand-clustered-data-plot.png"
        BSL.writeFile f1 $ CSV.encode $ mconcat $ map (\x -> x ++ [[]]) cls
        _ <- GE.plot (GE.PNG f2)  $ listPlot' pnts
        _ <- GE.plot (GE.PNG f3) $ map listPlot (cls)
        putStrLn $ "\nPrinting results to: \n" ++ f1 ++ "\n" ++ f2 ++ "\n" ++ f3 ++ "\n"
           where
              listPlot = GE.Data2D [GE.Title "", GE.Style GE.Dots] [] . getScoreL
              listPlot' = GE.Data2D [GE.Title "", GE.Style GE.Dots] [] . map (\(_,y) -> y)


