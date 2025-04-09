{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Prob17(prob17) where

import Utils
import Data.Map qualified as M
import Data.MemoTrie


parse :: String -> (Id, Staircase)
parse l = (read $ tail $ ws!!0, ((read $ ws!!2, read $ ws!!4), (pp $ ws!!7, pp $ ws!!9)))
  where
    ws = words l
    pp "START" = startId
    pp "END" = endId
    pp x = read $ tail $ x


startStep, endStep :: Integer
startStep = 0
endStep = 100

startId, endId :: Id
startId = 0
endId = 151

type Id = Int -- The ID of a staircase
type Staircases = M.Map Id Staircase
type Staircase = ((Integer, Integer), (Id,Id)) -- (hi, lo) (from, to)
type Position = (Id, Integer) -- (staircaseID, step)
type Path = [Position]

showId :: Id -> String
showId i = "S" ++ show i

showPosition :: Position -> String
showPosition (pid, pint) = showId pid ++ "_" ++ show pint

showPath :: Path -> String
showPath p = intercalate "-" $ showPosition <$> p


countPaths1 :: [Integer] -> Integer -> Integer
countPaths1 steps = memo \target -> if target == 0 then 1 
                                    else sum $ (countPaths1 steps . ((-) target)) <$> filter (\s -> s<=target) steps

countPaths2 :: [Integer] -> Staircases -> Position -> Integer
countPaths2 stepsSizes staircases = go
  where
    go = memo \pos -> 
      let nextPositions :: [Position]
          nextPositions = nub $ concatMap (next staircases pos) stepsSizes
      in 
          if pos == (1,endStep) then 1 else sum $ (\p@(pid,s) -> if pid==endId then go (1,s) else go p) <$> nextPositions 
  

-- Works out next positions step distance away
next :: Staircases -> Position -> Integer -> [Position]
next staircases p@(pos, step) stepSize
  | stepSize == 0 = [p]
  | null branches = thisStaircase
  | otherwise = thisStaircase ++ concatMap (\(nid, ((nlo,_),_)) -> next staircases (nid, nlo) (stepSize - (nlo - step) - 1)) branches
  where
    ((_, hi), (_, to)) = staircases M.! pos
    
    thisStaircase 
      | (step + stepSize) <= hi = [(pos, step + stepSize)] 
      | pos == 1 = []
      | otherwise = next staircases (to, hi) (stepSize - (hi - step) - 1)
    
    branches :: [(Id, Staircase)]
    branches = M.toList $ M.filter (\((lo,_), (from,_)) -> (from==pos)&&(lo>=step)&&(stepSize>(lo-step))) staircases
    

-- Finds the (almost) alphabetically last path from a position to the target
lastPath :: Integer -> [Integer] -> Staircases -> Position -> Path
lastPath targetRank stepsSizes staircases p = getItem (p, targetRank)
  where
    getItem :: (Position, Integer) -> Path
    getItem (pos, rank)
      | null children = [pos]
      | rank>=total = pos : getItem (last children) -- rank is too high - just take the last path
      | otherwise = pos : getItem (findChild rank children)
      where
        total = routeCount pos
        children :: [(Position, Integer)]
        children = nextPositions pos

        findChild :: Integer -> [(Position, Integer)] -> (Position, Integer)
        findChild _ [] = error "Where did the last one go?"
        findChild target ((lp,li):ps)
          | null ps = (lp, target)
          | target > li = findChild (target - li) ps
          | otherwise = (lp, target)


    routeCount :: Position -> Integer
    routeCount = memo \pos -> 
      let next = nextPositions pos
      in 
        if pos == (1, endStep) || pos == (endId, endStep) then  1
        else sum $ snd <$> next
        
    nextPositions :: Position -> [(Position, Integer)]
    nextPositions pp = (\pp@(idd,ss) -> (pp,) $ if idd==151 then routeCount (1,ss) else routeCount pp) <$> kids
      where
        kids = sort $ nub $ concatMap (next staircases pp) stepsSizes


prob17 :: IO ()
prob17 = do
  ss <- getF lines 17
  let ps = splitOn "" ss 
      staircases = M.fromList $ parse <$> (ps!!0)
      moves :: [Integer]
      moves = (\s -> read s) <$> drop 3 (words (filter (/=',') $ head $ ps!!1))
      ((_,target),_) = staircases M.! 1

  putStrLn $ "Prob17: part1: " ++ show (countPaths1 moves target)
  putStrLn $ "Prob17:2 part2: " ++ show (countPaths2 moves staircases (1, startStep)) 
  putStrLn $ "Prob17:2 part3: " ++ show (showPath $ lastPath 100000000000000000000000000000 moves staircases (1, startStep)) 
  
  return ()

