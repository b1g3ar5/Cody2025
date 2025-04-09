module Prob13(prob13) where

import Utils (sortOn, Down(Down), getF) 
import Data.Map qualified as M
import Data.PQueue.Prio.Min qualified as Q
import Data.Set qualified as S

type Node = String
type Edge = (Node, Node, Int)
type Graph = (S.Set Node, M.Map Node (M.Map Node Int))


parse :: String -> Edge
parse l = (ws!!0, ws!!2, read $ ws!!4)
  where
    ws = words l
  

makeGraph :: [Edge] -> Graph
makeGraph = go (S.empty, M.empty)
  where
    go :: Graph -> [Edge] -> Graph
    go gr [] = gr
    go (ns, mp) ((a,b,d):es) = go (S.insert a $ S.insert b ns, M.insertWith (M.union) a (M.singleton b d) mp) es


solve :: Node -> Node -> (Node -> Node -> Int) -> Graph -> M.Map Node Int
solve start finish costFn (ns, gr) = dijkstra (Q.singleton 0 start) M.empty 
  where
    nextStates :: Node -> [Node]
    nextStates pos = case gr M.!? pos of
                      Nothing -> []
                      Just x -> fst <$> M.toList x

    dijkstra :: Q.MinPQueue Int Node -> M.Map Node Int -> M.Map Node Int
    dijkstra pipeline visited
      | Q.null pipeline = visited
      | state == finish = M.insert state savedMin visited
      | state `M.member` visited = dijkstra remainingPipeline newVisited
      | otherwise = dijkstra newPipeline newVisited
      where
        ((savedMin, state), remainingPipeline) = Q.deleteFindMin pipeline
        newStates = nextStates state
        newPipeline = remainingPipeline `Q.union` Q.fromList ((\n -> (savedMin + costFn state n, n)) <$> newStates)
        newVisited = M.insert state savedMin visited


findLongestCycle :: M.Map Node (M.Map Node Int) -> Node -> Int
findLongestCycle gr start = longest [] 0 start
  where
    longest visited len pos
      | pos `M.notMember` gr = 0
      | null visited = maximum mx -- to cope with th initial call
      | pos == v = len -- this means it's a loop!
      | pos `elem` vs = 0 -- not a proper loop
      | otherwise = if null mx then 0 else maximum mx
      where
        (v:vs) = visited
        mx = (\(n,d) -> (longest (visited ++ [pos]) (d+len) n)) <$> M.toList (gr M.! pos)    


prob13 :: IO ()
prob13 = do
  ss <- getF lines 13
  let es = parse <$> ss
      g1@(ns, mp) = makeGraph es
      
  putStrLn $ "Prob13: part1: " ++ show (product $ take 3 $ sortOn Down $ S.toList $ S.map (\n -> (solve "STT" n (\_ _ -> 1) g1) M.! n) ns)
  putStrLn $ "Prob13: part2: " ++ show (product $ take 3 $ sortOn Down $ S.toList $ S.map (\n -> (solve "STT" n (\a b -> (mp M.! a) M.! b) g1) M.! n) ns)
  putStrLn $ "Prob13: part3: " ++ show (maximum $ (S.toList $ S.map (\n -> findLongestCycle mp n) ns))

  return ()

