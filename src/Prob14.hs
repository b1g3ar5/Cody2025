{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Prob14(prob14) where

import Utils 
import Data.List
import Data.Set qualified as S
import Data.Map qualified as M
import Data.Array qualified as A


type Item = (Int, Int, Int)


parse :: String -> Item
parse s = (read $ init $ ws!!5, read $ init $ ws!!8, read $ ws!!12)
  where
    ws = words s


knapsack :: [Item] -> Int -> (Int, Int)
knapsack items totalMoney = table A.! (nitems, totalMoney)
  where
    nitems  = length items
    -- Note: start at 1
    values  = A.listArray (1, nitems) $ (\(q,_,u) -> (q,-u)) <$> items
    costs = A.listArray (1, nitems) $ (\(_,c,_) -> c) <$> items  
    bnds    = ((0, 0), (nitems, totalMoney))
    table :: A.Array (Int, Int) (Int, Int)
    table   = A.array bnds [(ij, profit ij) | ij <- A.range bnds]
    profit (itemix, money)
      | itemix == 0 = 0 
      | money == 0 = 0
      | cost > money = table A.! (itemix-1, money) -- can't afford this item
      | otherwise = max (value + table A.! (itemix-1, money - cost)) (table A.! (itemix-1, money))
      where
        value = values A.! itemix
        cost = costs A.! itemix


prob14 :: IO ()
prob14 = do
  ss <- getF lines 14
  let items = parse <$> ss
      
  putStrLn $ "Prob14: part1: " ++ show ( sum $ (\(_,_,u) -> u) <$> take 5 (sortOn Down $ items))
  putStrLn $ "Prob14: part2: " ++ show ( (\(a,b) -> -a*b) $ knapsack items 30)
  putStrLn $ "Prob14: part3: " ++ show ( (\(a,b) -> -a*b) $ knapsack items 300)

  return ()

