module Prob2(prob2) where

import Utils

funA, funB, funC, funABC :: Int -> Int
funA x = x+550
funB x = x*65
funC x = x^3
funABC = funA . funB . funC


prob2 :: IO ()
prob2 = do
  ss <- getLines 2
  let ns :: [Int]
      ns = read <$> drop 4 ss
      n = length ns
      median = (sort ns) !! (n `div` 2)
      evens = filter even ns
      inBudget = filter ((< 15000000000000) . funABC) ns 

  putStrLn $ "Prob2: part1: " ++ show (funABC $ median)
  putStrLn $ "Prob2: part2: " ++ show (funABC . sum $ evens)
  putStrLn $ "Prob2: part3: " ++ show (maximum inBudget)

  return ()


