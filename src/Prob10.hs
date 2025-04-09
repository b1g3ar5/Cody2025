{-# LANGUAGE LambdaCase #-}
{-# Language BlockArguments #-}


module Prob10(prob10) where

import Utils 
import Data.PQueue.Prio.Min qualified as Q
import Data.Map qualified as M
import Data.MemoTrie


getRow, getCol :: [(Coord, Int)] -> Int -> [Int]
getRow g ix = snd <$> filter (\((x,y),n) -> y==ix) g
getCol g ix = snd <$> filter (\((x,y),n) -> x==ix) g

search grid start finish = go finish
  where
    go :: Coord -> Int
    go = memo \p -> 
      case p of
        (0,0) -> grid M.! p
        (0,y) -> grid M.! p + go (0,y-1)
        (x,0) -> grid M.! p + go (x-1, 0)
        (x,y) -> grid M.! p + min (go (x-1,y)) (go (x, y-1))
	
	
prob10 :: IO ()
prob10 = do
  ss <- getLines 10
      
  let g :: [(Coord, Int)]
      g = parseGridWith (\c -> read [c]) $ filter (/=' ')  <$> ss
      rows = sum . getRow g <$> [0..49]
      cols = sum . getCol g <$> [0..49]
  putStrLn $ "Prob10: part1: " ++ show (minimum $ rows ++ cols)
  putStrLn $ "Prob10: part2: " ++ show (search (M.fromList g) (0,0) (14,14))
  putStrLn $ "Prob10: part3: " ++ show (search (M.fromList g) (0,0) (49,49))

  return ()

