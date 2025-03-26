module Prob5(prob5) where

import Utils ( sort, getLines, manhattan, Coord ) 
import Data.List ( delete )


parse :: String -> Coord
parse l = (read $ init $ tail $ ws!!0, read $ init $ ws!!1)
  where
    ws = words l


findNearest :: Coord -> [Coord] -> (Coord, Int)
findNearest p ps = (fst $ head $ sort $ filter ((== minDist) . snd) $ zip ps ds, minDist)
  where
    ds = manhattan p <$> ps
    minDist = minimum ds


visit :: Coord -> [Coord] -> Int
visit _ [] = 0
visit p is = minDist + visit next (delete next is)
  where
    (next, minDist) = findNearest p $ delete p is


prob5 :: IO ()
prob5 = do
  ss <- getLines 5
  let positions = parse <$> ss
      distances = manhattan (0,0) <$> positions
      (nearest,minDist) = findNearest (0,0) positions
      
  putStrLn $ "Prob5: part1: " ++ show (maximum distances - minDist)
  putStrLn $ "Prob5: part2: " ++ show (snd $ findNearest nearest $ delete nearest positions)
  putStrLn $ "Prob5: part3: " ++ show (visit (0,0) positions)

  return ()

