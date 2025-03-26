module Prob7(prob7) where

import Utils 


parse :: [String] -> ([Int], [(Int, Int)], Int)
parse ls = (read <$> ps!!0, (\ls -> (read $ ls!!0, read $ ls!!1)) . (splitOn '-') <$> ps!!1, read $ (ps!!2)!!0)
  where
    ps = splitOn "" ls
   

swap1 :: [Int] -> (Int, Int) -> [Int]
swap1 xs (a, b) = zipWith (\x ix -> if ix==a then xs!!(b-1) else if ix==b then xs!!(a-1) else x) xs [1..]


swap2 :: [Int] -> (Int, Int, Int) -> [Int]
swap2 xs (a, b, c) = zipWith go xs [1..]
  where
    go x ix
      | ix==a = xs!!(c-1)
      | ix==b = xs!!(a-1)
      | ix==c = xs!!(b-1)
      | otherwise = x


swap3 :: [Int] -> (Int, Int) -> [Int]
swap3 xs (a, b) = zipWith go xs [1..]
  where
    n = length xs
    ((la, lb), (ha, hb)) = ((a, min (b-1) (a + n - b)), (b, min n (b+b-a-1)))
    go x ix
      | ix<la = x
      | ix<=lb = xs!!(ha+ix-la-1)
      | ix<ha = x
      | ix<=hb = xs!!(la+ix-ha-1)
      | otherwise = x


process :: ([Int] -> (Int, Int) -> [Int]) -> [Int] -> [(Int, Int)] -> Int -> Int
process fn ns swaps ix = (foldl fn ns swaps) !! (ix-1)


process2 :: [Int] -> [(Int, Int)] -> Int -> Int
process2 ns swaps resix = (foldl swap2 ns $ newSwaps) !! (resix-1)
  where
    n = length swaps
    newSwaps = ((\ix -> (fst $ swaps!!ix, snd $ swaps!!ix, fst $ swaps!!(ix+1))) <$> [0 .. n - 2]) 
                    ++ [(fst $ swaps!!(n-1), snd $ swaps!!(n-1), fst $ swaps!!0)]


tsort :: Ord b => (b, b) -> (b, b)
tsort (a,b) = if a<=b then (a,b) else (b,a)


prob7 :: IO ()
prob7 = do
  ss <- getLines 7
  let (ns, swp, ix) = parse ss
      
  putStrLn $ "Prob7: part1: " ++ show (process swap1 ns swp ix)
  putStrLn $ "Prob7: part2: " ++ show (process2 ns swp ix)
  putStrLn $ "Prob7: part3: " ++ show (process swap3 ns (tsort <$> swp) ix)

  return ()

