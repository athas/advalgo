module Main where

import Data.List

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

better :: Solution -> Maybe Solution
better s = find ((<cost s) . cost) $ mutations s

best :: Integer -> Solution -> Solution
best 0 s = s
best i s = maybe s (best (i-1)) $ better s

main :: IO ()
main = print $ cost (best 1000 (candidate 0.0 points))
