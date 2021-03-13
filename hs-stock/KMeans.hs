-- --------------------------------------------------------------------
-- | KMeans Module
-- --------------------------------------------------------------------
-- |    library of functions for performing kmeans algorithm on
-- |    Matrix type
-- --------------------------------------------------------------------

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleInstances #-}

module KMeans where
import System.Random
import Data.List
import Matrix
import qualified Control.Monad.Random as CMR
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import Debug.Trace 

-- --------------------------------------------------------------------
-- | KMeans Core functions
-- --------------------------------------------------------------------
-- |

-- | kmeans:
-- |    first select k centroids (centroid is a randomly selected Adj for each Vector)
-- |    run kmeans iteration with Vectors and centroids.
-- |    The output is a list of clusters (Matrix type).
-- --------------------------------------------------------------------
-- |    k = numbers of cluster to form
-- |    vs = data points (a Matrix containing adjacencies)
kmeans :: CMR.MonadRandom m => Int -> Matrix -> m [Matrix]
kmeans k vs 
    = let cs = take k <$> randomEls vs
        in kmeansIter vs <$> cs

-- | kmeansIter:
-- |    Top level iterator function for kmeans algorithm. First perform
-- |    iteration function until the centroids reach a fixed point.
-- |    And then do one final cluster with the fix point centroids.
-- --------------------------------------------------------------------
-- |    vs = data points 
-- |    cs = centroids
kmeansIter :: Matrix -> Matrix -> [Matrix]
kmeansIter vs cs 
    = clusterise vs $ (fixPnt (\x -> iteration vs x) cs)

-- | iteration:
-- |    Main iteration function for performing clusterise function, followed by 
-- |    the centroid update function.
-- --------------------------------------------------------------------
-- |    vs = data points 
-- |    cs = centroid
iteration :: [Vector] -> [Vector] -> [Vector]
iteration vs cs 
    = map centroidFn $ clusterise vs cs

-- | clusterise:
-- |    Clusters points in vs based of centroids centrs using the nearestTo
-- |    function to decide if a vector will be put in a cluster.
-- --------------------------------------------------------------------
-- |    vs = data points 
-- |    centrs = centroids
clusterise :: Matrix -> Matrix -> [Matrix]
clusterise vs centrs 
    = let add x = M.insertWith (++) (centrs `nearestTo` x) [x]
          m0    = M.unions $ map (\x -> x `M.singleton` []) centrs
        in M.elems $ (foldr add m0 vs)

-- | fixPnt:
-- |    Function which recursively applies the given function (iterFn) 
-- |    to the given points (pnts), updating them with each recursive
-- |    call and checking if have converged to a fixed point.
-- --------------------------------------------------------------------
-- |    iterFn = function that takes pnts type and returns pnts type
-- |    pnts = data points 
fixPnt :: (Eq a) => (a -> a) -> a -> a
fixPnt iterFn pnts 
    = let newPnts = iterFn pnts 
        in if pnts /= newPnts then fixPnt iterFn newPnts
           else newPnts 

-- | nearestTo:
-- |    Find the nearest vector to a given vector out of 
-- |    a list of vectors (matrix type).
-- --------------------------------------------------------------------
-- |    vs = list of vectors
-- |    x = given vector (to find which is nearest to this one)
nearestTo :: [Vector] -> Vector -> Vector
nearestTo vs x 
    = minimumBy (comparing (distanceFn x)) vs

-- | distanceFn:
-- |    Function for comparing Vectors distance from one another.
-- --------------------------------------------------------------------
-- |    a = first vector
-- |    b = second vector
distanceFn :: Vector -> Vector -> Adj 
distanceFn a b 
    = vectorSum $ zipWith distFn a b
        where
            distFn :: Adj -> Adj -> Adj
            distFn x y = let t = x - y
                           in t * t       -- == (a-b)^2

-- | centroidFn:
-- |    Update Matrix of centroids with their new mean
-- --------------------------------------------------------------------
-- |    vs = list of vectors (the centroid list)
centroidFn :: [Vector] -> Vector
centroidFn vs = map vectorMean (transpose vs)

-- --------------------------------------------------------------------
-- | Generators Functions (for testing)
-- --------------------------------------------------------------------
-- |
    
-- | randomEls:
-- |    randomly selects indexes from a given list
-- |    returns a list of 
-- |    return list of random numbers b/w range zero and length of the list
-- --------------------------------------------------------------------
-- |
randomEls :: CMR.MonadRandom m => [a] -> m [a]
randomEls xs 
    = trace "RR" $ map (\x -> xs !! x) 
                <$> CMR.getRandomRs (0, (length xs)-1)


-- | mkCluster:
-- |    Generate a list of data that is roughly clustered. These data points
-- |    can have k dimensions of sub-data points.
-- |    The raw data is generated randomly centered around a Arc Tangent function.
-- --------------------------------------------------------------------
-- |
mkCluster :: (CMR.MonadIO f, CMR.MonadRandom f) => Int -> Int -> Double -> Double -> f [[Double]]
mkCluster n k s p0
    = do
        seed  <- newStdGen
        let l = [p0] ++ randomlist (k-1) seed
            m' = mapM randomsAround l
        (take n . transpose) <$> m'
            where 
                randomsAround x0 = map (\x -> x0+s*atanh x) <$> CMR.getRandomRs (-1,1)
