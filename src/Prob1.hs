module Prob1(prob1) where

import Utils


accumulate :: Int -> [(Char, Int)] -> Int
accumulate = foldl (\acc (op, n) -> if op=='+' then acc + n else acc - n)


parse :: (String -> String) -> ([Int] -> [Int]) -> [String] -> Int
parse f1 f2 ls = accumulate (head ns) $ zip (f1 ops) $ tail $ ns
  where
    (ops, ns) = second f2 $ (last ls, read <$> init ls)


make2digit :: [Int] -> [Int]
make2digit ns = (\xs -> 10 * (xs!!0) + xs!!1) <$> chunksOf 2 ns


prob1 :: IO ()
prob1 = do
  ss <- getLines 1

  putStrLn $ "Prob1: part1: " ++ show (parse id id ss)
  putStrLn $ "Prob1: part2: " ++ show (parse reverse id ss)
  putStrLn $ "Prob1: part3: " ++ show (parse reverse make2digit ss)

  return ()


