{-# LANGUAGE PackageImports #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Ord
import Data.List
import qualified Data.Set as S

import System.Environment
import System.Random
import "random-shuffle" System.Random.Shuffle

type Point = (Double, Double)

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

points :: [Point]
points = [(1.797,0.397),
          (1.653,0.776),
          (1.430,0.883),
          (1.004,0.377),
          (1.057,0.577),
          (1.377,0.703),
          (0.790,1.043),
          (0.784,0.763),
          (2.530,0.730),
          (2.744,0.790),
          (2.530,0.397),
          (2.824,0.610),
          (1.624,1.277),
          (1.417,1.390),
          (1.184,1.510),
          (0.824,1.703),
          (2.070,1.083),
          (2.004,1.757),
          (2.351,1.173),
          (2.264,1.373),
          (2.378,1.313),
          (2.658,1.639),
          (2.565,2.476),
          (2.589,2.278),
          (2.114,2.185),
          (2.072,2.330),
          (2.225,2.258),
          (1.938,1.847),
          (1.815,1.811),
          (1.654,2.015),
          (1.759,1.983),
          (1.310,2.011),
          (1.326,2.658),
          (1.219,2.880),
          (1.512,2.813),
          (2.320,2.991),
          (2.314,3.389),
          (2.191,3.339),
          (1.797,3.941),
          (2.644,4.292),
          (3.577,4.320),
          (4.177,2.696),
          (3.852,2.389),
          (4.199,1.702),
          (2.807,2.300),
          (2.774,2.020),
          (2.761,1.900),
          (2.277,1.837),
          (2.230,1.970),
          (2.530,1.930),
          (2.670,1.790),
          (1.633,1.498),
          (1.787,1.331),
          (1.827,1.431),
          (2.557,0.250),
          (2.290,0.050),
          (2.237,0.437),
          (2.084,0.717),
          (2.417,0.917),
          (2.217,0.917),
          (1.552,1.079),
          (1.257,1.217),
          (1.404,1.277),
          (0.937,1.477),
          (0.877,1.230),
          (0.650,1.677),
          (0.157,1.910),
          (0.550,2.037),
          (1.024,1.763),
          (1.197,1.830),
          (1.370,1.683),
          (1.524,1.750),
          (1.524,1.617),
          (1.677,1.663),
          (1.890,2.317),
          (2.072,2.530),
          (2.841,3.329),
          (0.510,1.423),
          (2.037,0.870),
          (1.217,2.205) ]

type Solution = [Point]

-- | Generate a feasible solution.
candidate :: Double -> [Point] -> Solution
candidate _ [] = []
candidate d (p:ps) = p : candidate d (filter ((>d) . dist p) ps)

-- | Return the cost (total edge length) of a feasible solution.
cost :: Solution -> Double
cost [] = 0
cost s@(_:xs) = sum $ zipWith dist s xs

type Mutator = Solution -> [Solution]

swaps :: Mutator
swaps [] = []
swaps s@(p:ps) = swaps' [] s ++ map (p:) (swaps ps)
    where swaps' pre [] = []
          swaps' pre post@(p:ps) =
            swap pre post : swaps' (pre++[p]) ps
          swap = (++) . reverse

mutators :: [Mutator]
mutators = [swaps]

mutations :: Solution -> [Solution]
mutations s = concatMap ($s) mutators

n :: Int
n = 1000

bestFrom :: RandomGen g => [Solution] -> Solution -> g -> Solution
bestFrom taboo s g = case sortBy (comparing cost) (take n sols') of
                       []     -> s
                       (s':_) -> s'
  where sols  = [ s' | s' <- mutations s, not (s' `elem` taboo) ]
        sols' = shuffle' sols (length sols) g

taboo :: (Functor m, Monad m, RandomGen g) =>
         m g -> Int -> Integer -> Solution -> m [Solution]
taboo gm l i s = taboo' i [s] s s
    where taboo' 0 _ best s     = return [s]
          taboo' i seen best s  = do
            s' <- liftM (bestFrom seen s) gm
            let best' | cost s' < cost best = s'
                      | otherwise           = best
            (s:) <$> taboo' (i-1) (take l $ s' : seen) best' s'

start = (candidate 0.2 points)

main :: IO ()
main = do args <- getArgs
          case args of
            [runs, d, l] -> do
              res <- taboo newStdGen (read l) (read runs) (candidate (read d) points)
              case res of
                (_:_) -> do
                  let best = minimumBy (comparing cost) res
                  putStrLn $ "Best: " ++ show (cost best)
                  putStrLn $ "Average: " ++ show (sum (map cost res) / fromIntegral (length res))
                _        -> error "no solution"
            _   -> error "usage: taboo runs d l"
