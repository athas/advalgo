-- % ghc --make copenhagenGraph.hs

import Data.List
import System.Environment

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

pluses = intercalate " + "
minuses = intercalate " - "
pr con i = " c" ++ show i ++ ": " ++ con
indent   = map (" "++)

maxint d =    "Maximize\n obj: " ++ objfun ++ "\n"
           ++ "Subject To\n" ++ unlines (zipWith pr (constraints d) [1..])
           ++ "Bounds\n" ++ unlines (indent bounds)
           ++ "Integer\n" ++ unlines (indent $ map (uncurry flow . fst) edges)
           ++ "End\n"
    where objfun = intercalate " + " $ map f edges
              where f ((u, v), w) = show w ++ " " ++ flow u v
          bounds =    map (cap . fst) edges
                   ++ map nonneg edges
              where cap (u, v) = flow u v ++ " <= 1"
                    nonneg     = (++" >= 0") . uncurry flow . fst
          constraints d =    map cap edges
                          ++ map flowc [1..34]
                          ++ [outflow ++ " - " ++ inflow ++ " = " ++ show d]
              where flowc u =    pluses (map (flow u . snd . fst) $ edgesFrom u)
                              ++ " - "
                              ++ minuses (map (flip flow u . fst . fst) $ edgesTo u)
                              ++ " = 0"
                    outflow         = pluses $ map (flow 0 . snd . fst) $ edgesFrom 0
                    inflow          = minuses $ map (flip flow 0 . fst . fst) $ edgesTo 0
                    cap ((u, v), _) = flow u v ++ " + " ++ flow v u ++ " <= 1"

maxpaths =    "Maximize\n obj: " ++ objfun ++ "\n"
           ++ "Subject To\n" ++ unlines (zipWith pr constraints [1..])
           ++ "Bounds\n" ++ unlines (indent bounds)
           ++ "Integer\n" ++ unlines (indent $ map (uncurry flow . fst) edges)
           ++ "End\n"
    where objfun = outflow ++ " - " ++ inflow
              where outflow = pluses  $ map (flow 0 . snd . fst) $ edgesFrom 0
                    inflow  = minuses $ map (flip flow 0 . fst . fst) $ edgesTo 0
          constraints =    map cap edges
                        ++ map flowc [1..34]
              where flowc u =    pluses (map (flow u . snd . fst) $ edgesFrom u)
                              ++ " - "
                              ++ minuses (map (flip flow u . fst . fst) $ edgesTo u)
                              ++ " = 0"
                    cap ((u, v), _) = flow u v ++ "<= 1"
          bounds =    map (cap . fst) edges
                   ++ map nonneg edges
              where cap (u, v) = flow u v ++ " <= 1"
                    nonneg     = (++" >= 0") . uncurry flow . fst

main = do args <- getArgs
          case args of
            ["maxint", d] -> putStr (maxint (read d :: Int))
            "maxint":_    -> putStr $ maxint 2
            ["maxpaths"]  -> putStr maxpaths
            _             -> error "usage: copenhagenGraph <maxint [d]|maxpaths>"
