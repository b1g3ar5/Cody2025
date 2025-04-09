{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prob18(prob18) where

import Utils
import Data.Set qualified as S


type State = (Int, Coord4)
type State2 = ((Int,Int), Coord4) -- time and health


inBounds :: Coord4 -> Bool
inBounds (x,y,z,a) = (x>=0) && (x<(dims!!0)) && (y>=0) && (y<(dims!!1)) && (z>=0) && (z<(dims!!2)) &&  (a>=(-1)) && (a<=1)


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


neighbours :: Coord4 -> [Coord4]
neighbours p = filter inBounds $ (+ p) <$> [(1,0,0,0), (-1,0,0,0), (0,1,0,0), (0,-1,0,0), (0,0,1,0), (0,0,-1,0), (0,0,0,0)]


-- The idea with this was to go back from each possible position with these vols
-- and then to see if (position, velocity) satisfied any deris condition that had that velicity
allVels :: [Coord4]
allVels = [(vx,vy,vz,va) | vx <- [-1,0,1], vy <- [-1,0,1], vz <- [-1,0,1], va <- [-1,0,1]]


solve :: Coord4 -> Coord4 -> [Int -> Coord4 -> Bool] ->  Maybe (Int, Coord4)
solve start finish debris = dijkstra nextStates costFn ((==finish) . snd) [(0, start)]
  where
    nextStates :: State -> [State]
    nextStates (t,p) = (t+1,) <$> filter (isSafe (t+1)) (neighbours p)
    
    isSafe :: Int -> Coord4 -> Bool
    isSafe _ (0,0,0,0) = True
    isSafe time place = and ((\pred -> not $ pred time place) <$> debris)

    costFn :: State -> State -> State
    costFn _ = id


solve2 :: Int -> Coord4 -> Coord4 -> [Int -> Coord4 -> Bool] ->  Maybe State2
solve2 startHealth start finish debris = dijkstra nextStates costFn ((==finish) . snd) [((0,startHealth), start)]
  where
    nextStates :: State2 -> [State2]
    nextStates ((time, health),place) = foldl go [] $ neighbours place
      where
        go acc nxt
          | hs <= health = ((time+1, health - hs), nxt) : acc
          | otherwise = acc
          where
            hs = hits (time+1)  nxt

    hits ::Int -> Coord4 -> Int
    hits _ (0,0,0,0) = 0
    hits time place = foldl (\acc pred -> acc + if pred time place then 1 else 0) 0 debris

    costFn :: State2 -> State2 -> State2
    costFn _ = id


dims :: [Int]
dims = [10,15,60, 3]


prob18 :: IO ()
prob18 = do
  ss <- getF lines 18
  let pvs = parse <$> ss
      debris = pvs
      finish = (dims!!0 - 1, dims!!1 - 1, dims!!2 - 1, 0)
      start = (0,0,0,0)

  putStrLn $ "Prob18: part1: " ++ show (sum $ (\p -> S.size $ S.filter (p 0) space) <$> debris)
  timeIt $ putStrLn $ "Prob18: part2: " ++ show (fst <$> solve start finish debris)
  timeIt $ putStrLn $ "Prob18: part3: " ++ show (fst . fst <$> solve2 3 start finish debris)

  return ()


