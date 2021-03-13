-- --------------------------------------------------------------------
-- | Matrix Module
-- --------------------------------------------------------------------
-- |    Module containing Matrix representation and various functions
-- |    to operate on this type.
-- --------------------------------------------------------------------

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Matrix where
import System.IO
import System.Random
import Data.List
import GHC.Generics
import Data.List.Index
import GHC.Float
import qualified Numeric.LinearAlgebra.Data as LAD (fromLists, toLists, toList) 
import qualified Numeric.LinearAlgebra as LA (eigenvalues)
import qualified Numeric.LinearAlgebra.HMatrix as LAHM (eigenvaluesSH', eigSH') 
import Debug.Trace
import qualified Data.Csv as CSV
import qualified Data.Text.Lazy as DTL
import Data.Char (isSpace)
import Debug.Trace

-- --------------------------------------------------------------------
-- | Data Representation (Types)
-- --------------------------------------------------------------------
-- |

-- | Matrix: 
-- |    Type representing the weighted adjacency Matrix
-- |    of a weighted graph.
-- |    Matrices are just lists of Vector type.
-- --------------------------------------------------------------------
type Matrix = [Vector]

-- | Vector: 
-- |    Type representing all adjacencies to a data point
-- |    in a weighted graph.
-- |    Vectors are just lists of Adj type (adjacent vertices)
-- --------------------------------------------------------------------
type Vector = [Adj]

-- | Adj: 
-- |    Type representing the weighted adjacency  b/w two vertices
-- |    in a weighted graph.
-- --------------------------------------------------------------------
-- |    e = edge (containing the vertices)
-- |    w = weight of the edge (similarity rating b/w 0 and 1)
data Adj = Adj 
    { e :: Edge
    , w :: Double
    }
instance Num Adj where
    -- Adjacency Addition
    (+) Adj{e=e1, w=w1} Adj{e=e2, w=w2} = Adj (e1+e2) (w1+w2)
    -- Adjacency Subtraction
    (-) Adj{e=e1, w=w1} Adj{e=e2, w=w2} = Adj (e1-e2) (w1-w2)
    -- Adjacency Multiplication
    (*) Adj{e=e1, w=w1} Adj{e=e2, w=w2} = Adj (e1*e2) (w1*w2)
    -- Adjacency Absolute value 
    abs Adj{e, w} = Adj e $ abs w
    -- Adjacency Sign of weight
    signum Adj{e, w} = Adj e $ signum w
    -- Adjacency conversion
    fromInteger i = Adj (emptyEdge) (fromInteger i)
instance Show Adj where
    show Adj{e, w} = show w ++ "(" ++ show e ++ ")"
instance Eq Adj where
    (==) Adj{e=e1, w=w1} Adj{e=e2, w=w2} = w1 == w2
instance Ord Adj where
    compare Adj{e=e1, w=w1} Adj{e=e2, w=w2} = compare w1 w2
instance CSV.ToField Adj where
    toField adj = CSV.toField $ DTL.strip $ DTL.pack $ show adj

-- | Edge: 
-- |    type representing the edge b/w two vertices in a graph.
-- --------------------------------------------------------------------
-- |    i = vertex 1
-- |    j = vertex 2
data Edge = Edge 
    { i :: Vertex
    , j :: Vertex
    }
instance Show Edge where
    show Edge{i, j} = show i ++ "to" ++ show j
instance Num Edge where
    -- Edge Addition
    (+) Edge{i=i1, j=j1} Edge{i=i2, j=j2} = Edge (i1+i2) (j1+j2)
    -- Edge Subtraction
    (-) Edge{i=i1, j=j1} Edge{i=i2, j=j2} = Edge (i1-i2) (j1-j2)
    -- Edge Multiplication
    (*) Edge{i=i1, j=j1} Edge{i=i2, j=j2} = Edge (i1*i2) (j1*j2)
    -- Edge Absolute value 
    abs v = v
    -- Edge Sign of weight
    signum v = v
    -- Edge conversion
    fromInteger i = emptyEdge

-- | Vertex: 
-- |    type representing a vertex in a graph.
-- --------------------------------------------------------------------
-- |    name = label of vertex
-- |    pos  = position in Adjacency Matrix
data Vertex = Vertex 
    { name :: String
    , pos  :: Int
    , score :: (Double, Double)
    } 
instance Show Vertex where
    show Vertex{name, pos, score} = show (name) ++ " " ++ show (pos) ++ " " ++ show (score)
instance Num Vertex where
    -- Vertex Addition
    (+) Vertex{name=name1, pos=pos1, score=score1} 
            Vertex{name=name2, pos=pos2, score=score2} 
        = Vertex (name1) (pos1) (score1)
    -- Vertex Subtraction
    (-) Vertex{name=name1, pos=pos1, score=score1} 
            Vertex{name=name2, pos=pos2, score=score2} 
        = Vertex (name1) (pos1) (score1)
    -- Vertex Multiplication
    (*) Vertex{name=name1, pos=pos1, score=score1} 
            Vertex{name=name2, pos=pos2, score=score2} 
        = Vertex (name1) (pos1) (score1)
    -- Vertex Absolute value 
    abs v = v
    -- Vertex Sign of weight
    signum v = v
    -- Vertex conversion
    fromInteger i = Vertex "" 0 (fromInteger i, (fromInteger i))

-- --------------------------------------------------------------------
-- | Construction / Conversion Functions
-- --------------------------------------------------------------------
-- |

emptyEdge :: Edge
emptyEdge = Edge emptyVertex emptyVertex

emptyVertex :: Vertex
emptyVertex = Vertex "" 0 (0,0)

-- | fromList:
-- |    convert list of pairs to Adjacency Matrix
-- |    pairs contain Vertex name and Vertex "Score" (for similarity weight)
-- --------------------------------------------------------------------
-- |    l   = vertex list, vertices are represented as pairs, like so: (name, score)
-- |    sig = sigma constant (for simFn)
fromList :: [(String, (Double,Double))] -> Double -> Matrix
fromList l sig
   = [[ newAdj (show (fst (l!!i)), i, snd (l!!i)) 
               (show (fst (l!!j)), j, snd (l!!j)) 
               sig
        | j <- [0..(length l)-1] ]
        | i <- [0..(length l)-1] ]

-- | toLists:
-- |    Convert Matrix type to a list of lists containing doubles, 
-- |    extracting the weights from the adjacency matrix.
-- --------------------------------------------------------------------
-- |    m = input matrix
toLists :: Matrix -> [[Double]]
toLists m = [map (\Adj{e,w} -> w) row | row <- m]


-- | newAdj:
-- |    create a adjacency, containing the Edge and the weight (strictly postive)
-- |    b/w two vertices.
-- --------------------------------------------------------------------
-- |    (name1, pos1, score1)  = tuple triplet containing the name, position
-- |                             and score of the first vertex
-- |    (name2, pos2, score2)  = tuple triplet containing the name, position
-- |                             and score of the second vertex
-- |    sig                    = sigma constant (for simFn)
newAdj :: (String, Int, (Double,Double)) -> (String, Int, (Double,Double)) -> Double -> Adj
newAdj (name1, pos1, score1) (name2, pos2, score2) sig
    = Adj (Edge (Vertex name1 pos1 score1) 
                (Vertex name2 pos2 score2)) 
           (if pos1==pos2 then 0 else (let s = simFn (squash score1) (squash score2) sig
                                         in if s >= 0 then s else 0) )
          where
            squash (s1, s2) = let i = abs (0.00000001*s1) + abs (0.001*s2)
                                in trace (show i) $ i

-- | identM:
-- |    create an Identity matrix from a given matrix (m), while preserving 
-- |    information from the input matrix.
-- --------------------------------------------------------------------
-- |    m = input matrix
identM :: Matrix -> Matrix
identM m = [[ ( let Adj{e, w} = (m!!i)!!j 
                in if i == j then Adj e 1 else Adj e 0)
            | j <- [0..(length (m!!i))-1] ]
            | i <- [0..(length m)-1] ]

-- | updM:
-- |    update matrix (m) weights with values contained in the 2d list (l)
-- --------------------------------------------------------------------
-- |    m = input matrix
-- |    l = 2d list containing weight updates
updM :: Matrix -> [[Double]] -> Matrix
updM m l = [[ let Adj{e, w} = (m!!i)!!j 
                in Adj e ( (l!!i)!!j )
            | j <- [0..(length (m!!i))-1] ]
            | i <- [0..(length m)-1] ]

-- | restoreM:
-- |    Update matrix (m) weights such that all adjacencies are connected
-- |    both ways
-- --------------------------------------------------------------------
-- |    m = input matrix
-- |    l = 2d list containing weight updates
restoreM :: Matrix -> Matrix
restoreM m = [[ ( let Adj{e, w} = (m!!i)!!j 
                in if w == 0 
                   then let Adj{e=e', w=w'} = (m!!j)!!i
                          in Adj e w'
                   else Adj e w)
            | j <- [0..(length (m!!i))-1] ]
            | i <- [0..(length m)-1] ]


-- | getScoreL:
-- |    Function for converting Matrix type into a type that can be
-- |    handled by GNUPlot API
-- --------------------------------------------------------------------
-- |    m = input matrix
getScoreL :: Matrix -> [(Double, Double)]
getScoreL m
    = concat $ 
      concat $ [[ ( let Adj{e, w} = (m!!i)!!j 
                        Edge{i=i1, j=j1} = e
                        Vertex{name=name1, pos=pos1, score=score1} = i1
                        Vertex{name=name2, pos=pos2, score=score2} = j1
                      in [score1, score2])
            | j <- [0..(length (m!!i))-1] ]
            | i <- [0..(length m)-1] ]

-- --------------------------------------------------------------------
-- | Spectral Analysis Functions
-- --------------------------------------------------------------------
-- |

-- | simFn: 
-- |    similarity function, calculates the similarity score of 
-- |    two Doubles
-- --------------------------------------------------------------------
-- |    x   = 1st score
-- |    y   = 2nd score
-- |    sig = sigma constant
--
simFn :: Double -> Double -> Double -> Double
simFn x y sig 
    = exp $ -abs((x-y)**2::Double)/(2*sig**2::Double)


-- | eig:
-- |    Calculate eigenvalues and eigenvectors with hmatrix library.
-- |    Output is list of eigenvalues paired with a Matrix of 
-- |    eigenvectors (as columns).
-- --------------------------------------------------------------------
-- |    m = input matrix
--
eig :: Matrix -> ([Double], [[Double]]) 
eig m 
    = let t = toLists m 
          (evals, evecs) = LAHM.eigSH' $ LAD.fromLists $ t 
        in (LAD.toList evals, LAD.toLists evecs)


-- --------------------------------------------------------------------
-- | Auxiliary Linear Algebra functions
-- --------------------------------------------------------------------
-- |

-- Scalar Multiplication (on Vector)
sdot :: Double -> Vector -> Vector
sdot n v = map (\x -> let Adj{..} = x in Adj e (n*w)) v

vsdiv :: Double -> Vector -> Vector
vsdiv n v = map (\x -> let Adj{..} = x in Adj e ((1/n)*w)) v

-- Scalar Multiplication (on Matrix)
msdot :: Double -> Matrix -> Matrix
msdot n m =  [ n `sdot` row | row <- m]

-- Vector Multiplication
(|*|) :: Vector -> Vector -> Vector
u |*| v = zipWith (*) u v

-- Vector Addition
(|+|) :: Vector -> Vector -> Vector
u |+| v = zipWith (+) u v

-- Matrix Multiplication
(||*||) :: Matrix -> Matrix -> Matrix
m1 ||*|| m2 = zipWith (|*|) m1 m2

-- Matrix-Vector Multiplication
(||*|) :: Matrix -> Vector -> Matrix
m ||*| v | numCols m == length v 
    = map (\x -> x |*| v) m

-- Matrix Addition
(||+||) :: Matrix -> Matrix -> Matrix
m1 ||+|| m2 = zipWith (|+|) m1 m2

cmpAdj :: Adj -> Adj -> Ordering
cmpAdj Adj{e=e1, w=w1} Adj{e=e2, w=w2} = compare w1 w2

ltAdj :: Adj -> Adj -> Bool
ltAdj Adj{e=e1, w=w1} Adj{e=e2, w=w2} = (<) w1 w2

numRows :: Matrix -> Double
numRows = int2Double . length

vlen :: Vector -> Double
vlen = int2Double . length

numCols :: Matrix -> Int
numCols = length . head

vecMinAdj :: Vector -> Adj 
vecMinAdj v = head $ sortBy cmpAdj v

matrixMean :: Matrix -> Vector
matrixMean m = (1/(numRows m)) `sdot` (matrixSum m)

vectorMean :: Vector -> Adj
vectorMean v = (1/(vlen v)) `sadot` (vectorSum v)

matrixSum :: Matrix -> Vector
matrixSum m = foldr1 (|+|) m

vectorSum :: Vector -> Adj
vectorSum v = foldVector ((+)) v

vdot :: Vector -> Vector -> Adj
vdot v u = vectorSum $ v |*| u

avdot :: Adj -> Vector -> Vector
avdot a v = map (\b -> b * a) v

sadot :: Double -> Adj -> Adj
sadot n a = let Adj{e, w} = a in
             (Adj e n) * a

-- | foldVector: 
-- |    Combine a vector (list of adjacencies) into a single Adjacency
-- |    provided a combine function for weights
-- --------------------------------------------------------------------
-- |    f = combination function
-- |    v = list of adjacencies (vector type)
--
foldVector :: (Adj -> Adj -> Adj) -> Vector -> Adj
foldVector f v = foldr1 f' v
    where   f' :: Adj -> Adj -> Adj
            f' a b = a `f` b 

-- --------------------------------------------------------------------
-- | Functions used in Random testing
-- --------------------------------------------------------------------
-- |

randomlist :: Int -> StdGen -> [Double]
randomlist n = take n . unfoldr (Just . random)

randomL :: Int -> StdGen -> [Int]
randomL n = take n . unfoldr (Just . random)

mkScoreL :: [String] -> [[Double]] -> [(String, (Double, Double))]
mkScoreL labelL scoreL
   = let ls = zipWith (,) labelL scoreL 
       in [ (l, mkScore s) | (l,s) <- ls ]
        where mkScore (x:y:_) = (10000000*x,1000*y)
