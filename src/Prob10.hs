module Prob10(prob10) where

import Utils 
import Data.PQueue.Prio.Min qualified as Q
import Data.Map qualified as M


getRow, getCol :: [(Coord, Int)] -> Int -> [Int]
getRow g ix = snd <$> filter (\((x,y),n) -> y==ix) g
getCol g ix = snd <$> filter (\((x,y),n) -> x==ix) g


dijkstra :: (Ord s) => Q.MinPQueue Int s -> M.Map s Int -> (s -> [s]) -> (s -> s -> Int) -> (s -> Bool) -> M.Map s Int
dijkstra pipeline visited nextFn costFn finishFn
  | Q.null pipeline = visited
  | finishFn state = newVisited
  | state `M.member` visited = dijkstra remainingPipeline newVisited nextFn costFn finishFn
  | otherwise = dijkstra newPipeline newVisited nextFn costFn finishFn
  where
    ((savedMin, state), remainingPipeline) = Q.deleteFindMin pipeline
    newStates = filter (`M.notMember` visited) $ nextFn state
    newPipeline = remainingPipeline `Q.union` Q.fromList ((\n -> (savedMin + costFn state n, n)) <$> newStates)
    newVisited = M.insertWith min state savedMin visited


search :: M.Map Coord Int -> Coord -> Coord -> Int
search grid start finish = (dijkstra (Q.singleton (grid M.! start) start) M.empty nextFn costFn finishFn) M.! finish
  where
    nextFn (x,y) = filter (\(a,b) -> (a>=0)&&(b>=0)&&(a<50)&&(b<50)) [(x+1, y), (x, y+1)]
    finishFn = (== finish)
    costFn _ to = grid M.! to


prob10 :: IO ()
prob10 = do
  ss <- getLines 10
      
  let g :: [(Coord, Int)]
      g = parseGridWith (\c -> read [c]) $ filter (/=' ')  <$> ss
      rows = sum . getRow g <$> [0..49]
      cols = sum . getCol g <$> [0..49]
  putStrLn $ "Prob10: part1: " ++ show (minimum $ rows ++ cols)
  putStrLn $ "Prob10: part2: " ++ show (search (M.fromList g) (0,0) (14,14))
  putStrLn $ "Prob10: part3: " ++ show (search (M.fromList g) (0,0) (49,49))

  return ()

