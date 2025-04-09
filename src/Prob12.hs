module Prob12(prob12) where

import Utils 


data Dir = Col Int | Row Int | All deriving (Show, Eq)

parseDir :: String -> Dir
parseDir s
  | ws!!0 == "ALL" = All
  | ws!!0 == "ROW" = Row $ read $ ws!!1
  | ws!!0 == "COL" = Col $ read $ ws!!1
  | otherwise = error $ "Oi: " ++ show s
  where
    ws = words s


data OpType = Add | Sub | Mul | Shift deriving (Show, Eq)
type Op = (OpType, Dir, Int)


sub, add, mul :: Int -> Int -> Int
sub a b = (a - b) `mod` 1073741824
mul a b = (a * b) `mod` 1073741824
add a b = (a + b) `mod` 1073741824


parseOp :: String -> Op
parseOp s
  | ws!!0 == "ADD" = (Add, parseDir $ ws!!2++ " " ++ws!!3, read $ ws!!1)
  | ws!!0 == "SUB" = (Sub, parseDir $  ws!!2++ " "++ws!!3, read $ ws!!1)
  | ws!!0 == "MULTIPLY" = (Mul, parseDir $  ws!!2++ " "++ws!!3, read $ ws!!1)
  | ws!!0 == "SHIFT" = (Shift, parseDir $  ws!!1++ " "++ws!!2, read $ ws!!4)
  | otherwise = error "Oi!!"
  where
    ws = words s

evens :: [a] -> [a]
evens [] = []
evens [_] = []
evens (_:x:xs) = x : evens xs


parse :: [String] -> ([[Int]], [Op], [String])
parse ls = ((\l -> read <$> words l) <$> ps!!0, parseOp <$> ps!!1, evens $ ps!!2)
  where
    ps = splitOn "" ls


run :: [[Int]] -> Op -> [[Int]]
run ns (Add, Row x, n) = zipWith (\r ix -> if ix==x then (`add` n)<$>r else r) ns [1..]
run ns (Add, Col x, n) = transpose $ zipWith (\r ix -> if ix==x then (`add` n)<$>r else r) (transpose ns) [1..]
run ns (Add, All, n) = ((`add` n)<$>) <$> ns
run ns (Sub, Row x, n) = zipWith (\r ix -> if ix==x then (`sub` n)<$>r else r) ns [1..]
run ns (Sub, Col x, n) = transpose $ zipWith (\r ix -> if ix==x then (`sub` n)<$>r else r) (transpose ns) [1..]
run ns (Sub, All, n) = ((`sub` n)<$>) <$> ns
run ns (Mul, Row x, n) = zipWith (\r ix -> if ix==x then (`mul` n)<$>r else r) ns [1..]
run ns (Mul, Col x, n) = transpose $ zipWith (\r ix -> if ix==x then (`mul` n)<$>r else r) (transpose ns) [1..]
run ns (Mul, All, n) = ((`mul` n)<$>) <$> ns
run ns (Shift, Row x, n) = zipWith (\r ix -> if ix==x then drop (len - n) r ++ take (len - n) r else r) ns [1..]
  where
    len = length ns
run ns (Shift, Col x, n) = transpose $ zipWith (\r ix -> if ix==x then drop (len - n) r ++ take (len - n) r else r) (transpose ns) [1..]
  where
    len = length ns
run _ (Shift, All, n) = error "Can't Shift ALL"

perform :: [String] -> [Op] -> [[Int]] -> [[Int]]
perform [] _ ns = ns
perform _ [] ns = ns
perform (a:as) (op:ops) ns
  | a=="CYCLE" = perform as (ops ++ [op]) ns
  | a=="ACT" = perform as ops $ run ns op
  | otherwise = error $ "Unknown action: " ++ a


score1 :: [[Int]] -> Int
score1 ns = maximum $ (sum <$> transpose ns) ++ (sum <$> ns)


prob12 :: IO ()
prob12 = do
  ss <- getF lines 12

  let (ns, ops, actions) = parse ss
      
  putStrLn $ "Prob12: part1: " ++ show (score1 $ foldl run ns $ ops) 
  putStrLn $ "Prob12: part2: " ++ show (score1 $ perform actions ops ns)
  putStrLn $ "Prob12: part3: " ++ show (score1 $ perform (cycle actions) ops ns)

  return ()

