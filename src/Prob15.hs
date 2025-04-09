module Prob15(prob15) where

import Utils hiding (TreeF(..))


data TreeF a r = LeafF | NodeF a r r deriving (Functor, Show)


-- Parsing algebras

-- ID and the tree level
parse1 :: ([Int],Int) -> TreeF (Int, Int) ([Int], Int)
parse1 ([],_) = LeafF
parse1 (x:xs, lvl) = NodeF (x, lvl) (filter (<x) xs, lvl+1) (filter (>x) xs, lvl+1)


-- id and Code
parse2 :: [(String, Int)] -> TreeF (String, Int) [(String, Int)]
parse2 [] = LeafF
parse2 (l@(_,x):ls) = NodeF l (filter ((<x).snd) ls) (filter ((>x).snd) ls)

-- Just the code
parse3 :: [(String, Int)] -> TreeF String [(String, Int)]
parse3 [] = LeafF
parse3 (l@(s,x):ls) = NodeF s (filter ((<x).snd) ls) (filter ((>x).snd) ls)


-- Coalgebra to sum of ids on a level
levelCount :: Int -> TreeF (Int, Int) Int -> Int
levelCount _ LeafF = 0
levelCount lvl (NodeF (x, n) l r) = if n==lvl then x else l + r


-- Coalgebra to get the depth of a tree
depth :: TreeF a Int -> Int
depth LeafF = 0
depth (NodeF _ lt rt) = 1 + max lt rt


-- Coalgebra that calculates the route taken by a 500000 id
route :: TreeF (String, Int) String -> String
route LeafF = ""
route (NodeF (s,x) lt rt) = s ++ "-" ++ if x<500000 then rt else lt


-- Gets the commmon ancestor of a and b
ancester :: String -> String -> TreeF String (Maybe String) -> (Maybe String)
ancester _ _ LeafF = Nothing
-- Found nothing yet
ancester a b (NodeF s Nothing Nothing) = if s==a then Just a else if s==b then Just b else Nothing
-- If the branch has found one of the items ...
ancester a b (NodeF s (Just n) Nothing)
  | n==a = if s==b then Just b else Just a
  | n==b = if s==a then Just a else Just b
  | otherwise = Just n
-- If the branch has found the other item ...
ancester a b (NodeF s Nothing (Just n))
  | n==a = if s==b then Just b else Just a
  | n==b = if s==a then Just a else Just b
  | otherwise = Just n
-- Found both items in the branches - so this is the least common ancester
ancester _ _ (NodeF s (Just _) (Just _)) = Just s


prob15 :: IO ()
prob15 = do
  ss <- getF lines 15

  let ints = snd <$> t2
      t2 = (\l -> ((words l)!!0, read $ (words l)!!2)) <$> head (splitOn "" ss)
      descendants = (\l -> (words l)!!0) <$> (splitOn "" ss)!!1
      dep = hylo depth parse1 (ints, 0)
      a = descendants!!0
      b = descendants!!1
      
  putStrLn $ "Prob15: part1: " ++ show (dep * (maximum $ (\n -> hylo (levelCount n) parse1 (ints, 0)) <$> [0..100]))
  putStrLn $ "Prob15: part2: " ++ show (init $ hylo route parse2 t2)
  putStrLn $ "Prob15: part3: " ++ show (hylo (ancester a b) parse3 t2)

  return ()

