module Prob4(prob4) where

import Utils 


memory :: String -> Int
memory s = sum $ score <$> s
  where
    score c
      | c `elem` ['A'..'Z'] = ord c - ord 'A' + 1
      | otherwise = read [c]
  

lossy :: String -> String
lossy s = take keep s ++ show (l - 2 * keep) ++ drop (l-keep) s
  where
    l = length s
    keep = l `div` 10


lossless :: String -> String
lossless s = concatMap (\g -> show (length g) ++ take 1 g) $ group s


prob4 :: IO ()
prob4 = do
  ss <- getLines 4
      
  putStrLn $ "Prob4: part1: " ++ show ( sum $ memory <$> ss)
  putStrLn $ "Prob4: part2: " ++ show ( sum $ memory . lossy <$> ss)
  putStrLn $ "Prob4: part3: " ++ show ( sum $ memory . lossless <$> ss)

  return ()

