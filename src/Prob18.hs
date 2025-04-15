{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prob18(prob18) where

import Utils hiding (neighbours4)
import Data.Set qualified as S
import Data.Array qualified as A
import Data.MemoTrie


type Health = Int
type Time = Int
type State = ((Time,Health), Coord3) -- time and health


{-# INLINE inBounds3 #-}
inBounds3 :: Coord3 -> Bool
inBounds3 (x,y,z) = (x>=0) && (x<(dims!!0)) && (y>=0) && (y<(dims!!1)) && (z>=0) && (z<(dims!!2))


space :: S.Set Coord4
space = S.fromList [(x,y,z,a) | x <- [0..(dims!!0 - 1)], y <- [0..(dims!!1 - 1)], z <- [0..(dims!!2 - 1)], a <- [-1,0,1]]


parse :: String -> (Int -> Coord4 -> Bool)
parse l = makePred (read $ init $ ws!!2) (read $ init $ ws!!3) (read $ init $ ws!!4) (read $ init $ ws!!5) (read $ ws!!7) (read $ ws!!10)
          (read (init $ tail $ ws!!14)) (read $ init $ ws!!15) (read $ init $ ws!!16) (read $ init $ ws!!17)
  where
    ws = words $ (\c -> if c== '+' then ' ' else c) <$> l
    makePred :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int -> Coord4 -> Bool)
    makePred p q r s qt rm vp vq vr vs = \t (x,y,z,a) -> (
       p*((x-vp*t) `mod` 10)
      +q*((y-vq*t) `mod` 15)
      +r*((z-vr*t) `mod` 60)
      +s*(-1 + ((a-vs*t+1) `mod` 3))) `mod` qt == rm


{-# INLINE neighbours3 #-}
neighbours3 :: Coord3 -> [Coord3]
neighbours3 p = filter inBounds3 $ (+ p) <$> [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1), (0,0,0)]


solve2 :: Int -> Coord3 -> Coord3 -> [Int -> Coord4 -> Bool] -> State
solve2 startHealth start finish !debris = bfs nextStates ((==finish) . snd) ((0,startHealth), start)
  where
    nextStates :: State -> [State]
    nextStates ((time, health),place) = foldl' go [] $ neighbours3 place
      where
        go acc nxt
          | hs <= health = ((time+1, health - hs), nxt) : acc
          | otherwise = acc
          where
            hs = hits ((time+1) `mod` 60)  nxt

    hits :: Int -> Coord3 -> Int
    hits = memo2 \time p@(x,y,z) -> if p == (0,0,0) then 0 else length $ filter (\pred -> pred time (x,y,z,0)) debris


dims :: [Int]
dims = [10,15,60,3]


prob18 :: IO ()
prob18 = do
  ss <- getF lines 18
  let pvs = parse <$> ss
      !debris = pvs
      finish = (dims!!0 - 1, dims!!1 - 1, dims!!2 - 1)
      start = (0,0,0)

  putStrLn $ "Prob18: part1: " ++ show (sum $ (\p -> S.size $ S.filter (p 0) space) <$> debris)
  timeIt $ putStrLn $ "Prob18: part2: " ++ show (solve2 0 start finish debris) --198
  timeIt $ putStrLn $ "Prob18: part2: " ++ show (solve2 3 start finish debris) --198

  return ()


