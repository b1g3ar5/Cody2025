module Prob11(prob11) where

import Utils 

type Base = Int

type Number = ([Int], Base)


-- ! represents 62, @ represents 63, # represents 64, $ represents 65, % represents 66, and ^ represents 67
parseC :: Char -> Int
parseC '!' = 62
parseC '@' = 63
parseC '#' = 64
parseC '$' = 65
parseC '%' = 66
parseC '^' = 67
parseC c
  | c `elem` ['0'..'9'] = read [c]
  | c `elem` ['A'..'Z'] = ord c - ord 'A' + 10
  | c `elem` ['a'..'z'] = ord c - ord 'a' + 36
  | otherwise = error "Not a number"


showC :: Int -> Char
showC 62 = '!'
showC 63 = '@'
showC 64 = '#'
showC 65 = '$'
showC 66 = '%'
showC 67 = '^'
showC n
  | n>35 = chr $ n - 36 + ord 'a'
  | n>9 = chr $ n - 10 + ord 'A'
  | otherwise = head $ show n


parse :: String -> ([Int], Int)
parse l = (parseC <$> (ws!!0), read $ ws!!1)
  where
    ws = words l

convert :: Number -> Int
convert (ns, b) = foldl (\acc n -> acc*b+n) 0 ns

convert68 :: Int -> String
convert68 n
  | n==0 = ""
  | otherwise = (convert68 q) ++ [showC r]
  where
    (q,r) = n `quotRem` 68


converge :: Int -> Int
converge target = binSearch go 0 9999
  where
    go x = (x-1)*(x^3 + x^2 + x + 1) > target


prob11 :: IO ()
prob11 = do
  ss <- getLines 11
  --let ss = test
  let ls = parse <$> ss
      base10 = convert <$> ls
      
  putStrLn $ "Prob11: part1: " ++ show (maximum base10)
  putStrLn $ "Prob11: part2: " ++ show (convert68 $ sum base10)
  putStrLn $ "Prob11: part3: " ++ show (converge $ sum base10)

  return ()

test = ["32IED4E6L4 22"
  , "1111300022221031003013 4"
  , "1C1117A3BA88 13"
  , "1100010000010010010001111000000010001100101 2"
  , "7AJ5G2AB4F 22"
  , "k6IHxTD 61"]