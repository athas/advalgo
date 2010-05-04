-- % ghc --make copenhagenGraph.hs

import Data.List
import qualified Data.Map as M

edges = concatMap f $
        [(( 0, 1), 2),
         (( 0, 4), 2),
         (( 0, 5), 5),
         (( 0,34), 6),
         (( 1, 2), 1),
         (( 1, 3),22),
         (( 2, 6), 1),
         (( 2, 7),85),
         (( 3, 4), 7),
         (( 3, 7),95),
         (( 3, 9),84),
         (( 4,10),21),
         (( 5, 6), 7),
         (( 5,11), 3),
         (( 6,11),37),
         (( 6,13),20),
         (( 7, 8),77),
         (( 8, 9),72),
         (( 8,15),54),
         (( 9,10),11),
         ((11,12),30),
         ((11,33),13),
         ((11,34), 2),
         ((12,13),47),
         ((12,18), 8),
         ((12,33),18),
         ((13,14),12),
         ((13,17),29),
         ((14,15),39),
         ((14,16), 2),
         ((14,32),20),
         ((16,17), 7),
         ((16,25),14),
         ((16,32),16),
         ((17,19), 4),
         ((17,22),18),
         ((18,19),19),
         ((18,20),29),
         ((18,21), 2),
         ((19,20), 8),
         ((19,23), 7),
         ((20,21),10),
         ((20,24), 5),
         ((21,31),60),
         ((22,25),14),
         ((23,25), 9),
         ((23,27),10),
         ((23,29),12),
         ((24,29), 7),
         ((24,30),20),
         ((25,26),11),
         ((26,27),69),
         ((26,32),66),
         ((27,28), 4),
         ((28,35), 1),
         ((29,35), 4),
         ((30,31),50),
         ((30,35),10),
         ((31,35),25)]
    where f ((u, v), w) = [((u, v), w), ((v, u), w)]

edgesTo v = filter ((v==) . snd . fst) edges
edgesFrom u = filter ((u==) . fst . fst) edges

vs x | x < 10    = "0" ++ show x
     | otherwise = show x

flow u v = "f" ++ vs u ++ vs v

vertices = concatMap (\v -> map ((,)v) [0..35]) [0..35]

objfun = intercalate " + " $ map f edges
    where f ((u, v), w) = show w ++ " f" ++ vs u ++ vs v

bounds =    map (cap . fst) edges
         ++ nonneg
    where cap (u, v) = flow u v ++ " <= 1"
          nonneg     = map (\(u, v) -> flow u v ++ " >= 0") $ map fst edges
          
constraints =    map flowc [1..34]
              ++ [mflow]
              ++ cap
    where flowc u =    intercalate " + " (map (flow u . snd . fst) $ edgesFrom u)
                    ++ " - "
                    ++ intercalate " - " (map (flip flow u . fst . fst) $ edgesTo u)
                    ++ " = 0"
          mflow   = outflow ++ " - " ++ inflow ++ " = 2"
          outflow = intercalate " + " $ map (\v -> "f" ++ vs 0 ++ vs v) $ map (snd . fst)  $ edgesFrom 0
          inflow  = intercalate " - " $ map (\v -> "f" ++ vs v ++ vs 0) $ map (fst . fst) $ edgesTo 0
          cap     = map (\(u, v) -> flow u v ++ " + " ++ flow v u ++ " <= 1") $ map fst edges

prob =    "Maximize\n obj: " ++ objfun
       ++ "\nSubject To\n" ++ unlines (zipWith pr constraints [1..])
       ++ "Bounds\n" ++ unlines (map (" "++) bounds)
       ++ "Integer\n" ++ unlines (map (" "++) $ map (uncurry flow . fst) edges)
       ++ "End\n"
    where pr con i = " c" ++ show i ++ ": " ++ con

main = putStr prob
