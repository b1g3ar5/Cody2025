module Prob6(prob6) where

import Utils 


value2 :: Char -> Int
value2 c
  | c `elem` ['A'..'Z'] = ord c - ord 'A' + 27
  | c `elem` ['a'..'z'] = ord c - ord 'a' + 1
  | otherwise = 0


value3 :: (Char, Int) -> Int
value3 (c, bf)
  | c `elem` ['A'..'Z'] = ord c - ord 'A' + 27
  | c `elem` ['a'..'z'] = ord c - ord 'a' + 1
  | otherwise = (bf * 2 - 5) `mod` 52


prob6 :: IO ()
prob6 = do
  ss <- getLines 6
      
  putStrLn $ "Prob6: part1: " ++ show ( length $ filter (`elem` ['A'..'Z']++['a'..'z']) $ ss!!0)
  putStrLn $ "Prob6: part2: " ++ show ( foldl (\z c -> z + value2 c ) 0 $ ss!!0)
  putStrLn $ "Prob6: part3: " ++ show ( fst $ foldl (\(z,bf) c -> (z + value3 (c, bf), value3 (c, bf))) (0,0) $ ss!!0)

  return ()

