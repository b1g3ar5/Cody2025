module Prob3(prob3) where

import Utils ( getLines, splitOn )
import Data.Set qualified as S


parse :: String -> ((Int, Int), (Int, Int))
parse l = (ws!!0, ws!!1)
  where
    ws = getRange <$> words l
    getRange :: String -> (Int, Int)
    getRange s = (read $ ns!!0, read $ ns!!1)
      where
        ns = splitOn '-' s


-- Double count overlaps
rng1 :: ((Int, Int), (Int, Int)) -> Int
rng1 ((f1,t1), (f2,t2)) = t1-f1+1+t2-f2+1


-- Single count overlaps
rng2 :: ((Int, Int), (Int, Int)) -> Int
rng2 ((f1,t1), (f2,t2))
  | (t1 < f2) || (t2 < f1) = t1 - f1 + 1 + t2 - f2 + 1
  | otherwise = (max t1 t2) - (min f1 f2) + 1


uniqueSet :: ((Int, Int), (Int, Int)) -> S.Set Int
uniqueSet ((f1,t1), (f2,t2)) = S.fromList $ [f1..t1] ++ [f2..t2]


prob3 :: IO ()
prob3 = do
  ss <- getLines 3
  let rs :: [((Int, Int), (Int, Int))]
      rs = parse <$> ss
      sets = uniqueSet <$> rs

  putStrLn $ "Prob3: part1: " ++ show (sum $ rng1 <$> rs)
  putStrLn $ "Prob3: part2: " ++ show (sum $ rng2 <$> rs)
  putStrLn $ "Prob3: part3: " ++ show (foldl (\m ix -> max m $ S.size $ (sets!!ix) `S.union` (sets!!(ix+1))) 0 $ [0 .. length rs - 2])

  return ()

