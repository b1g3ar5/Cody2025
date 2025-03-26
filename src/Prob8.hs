module Prob8(prob8) where

import Utils 


process :: ([a] -> Bool) -> [a] -> [a]
process pred cs
  | null cs = cs
  | even (length cs) = concat ps
  | otherwise = head cs : concat ps
  where
    ps = filter pred $ chunksOf 2 cs
    qs = filter pred $ chunksOf 2 (tail cs)


process' :: ([a] -> Bool) -> [a] -> [a]
process' pred cs
  | null cs = cs
  | even (length cs) = head cs : concat ps ++ [last cs]
  | otherwise = concat ps ++ [last cs]
  where
    ps = filter pred $ chunksOf 2 $ tail $ init cs
    qs = filter pred $ chunksOf 2 $ init cs


delete2 :: [Char] -> Bool
delete2 [] = error "No characters"
delete2 [a,b]
  | a `elem` ['0'..'9'] && b `elem` ('-':['a'..'z']) = True
  | b `elem` ['0'..'9'] && a `elem` ('-':['a'..'z']) = True
  | otherwise = False
delete2 _ = error "There must be just 2 characters"


delete3 :: [Char] -> Bool
delete3 [] = error "No characters"
delete3 [a,b]
  | a `elem` ['0'..'9'] && b `elem` (['a'..'z']) = True
  | b `elem` ['0'..'9'] && a `elem` (['a'..'z']) = True
  | otherwise = False
delete3 _ = error "There must be just 2 characters"


score :: [Char] -> Int
score = length . filter (`elem` ['a'..'z'])


reduce :: Eq a => ([a] -> Bool) -> [a] -> [a]
reduce pred = steadyState go 
  where
    go = steadyState (process pred) . steadyState (process' pred)


prob8 :: IO ()
prob8 = do
  ss <- getLines 8
  let s1 = ss
      
  putStrLn $ "Prob8: part1: " ++ show (sum $ score <$> s1)
  putStrLn $ "Prob8: part2: " ++ show (sum $ length . (reduce (not . delete2)) <$> s1)
  putStrLn $ "Prob8: part3: " ++ show (sum $ length . (reduce (not . delete3)) <$> s1)

  return ()

