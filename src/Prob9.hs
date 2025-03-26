module Prob9(prob9) where

import Utils 
import Data.Map qualified as M
import Data.Map (Map)
import Data.Either (isLeft)


parse :: [String] -> (Map String Int, [(String , String, Int)])
parse s = (M.fromList $ (\p -> (\ws -> (ws!!0, read $ ws!!2)) $ words p) <$> ps!!0, (\p -> ((\ws -> (ws!!1, ws!!3, read $ ws!!5))) $ words p) <$> ps!!1)
  where
    ps = splitOn "" s


apply1 :: [(String , String, Int)] -> Map String Int -> Map String Int
apply1 [] bals  = bals
apply1 ((from, to, x):ts) bals = apply1 ts $ M.insertWith (\b y -> y-b) from x $ M.insertWith (+) to x $ bals


apply2 :: [(String , String, Int)] -> Map String Int -> Map String Int
apply2 [] bals  = bals
apply2 ((from, to, x):ts) bals = apply2 ts $ M.insertWith (\b y -> y-b) from xx $ M.insertWith (+) to xx $ bals
  where
    xx = min x $ bals M.! from


-- Account is either: Left balance | Right creditors
type Account = Either Int [(String, Int)]


deposit :: (String, Int) -> Map String Account -> Map String Account
deposit (to, amount) accs
  | isLeft toAccount = M.insert to (Left $ balance + amount) accs
  | null creditors = M.insert to (Left amount) accs
  | amount >= owed = deposit (to, amount - owed) $ deposit (creditor, owed) $ M.insert to (Right otherCreditors) $ accs
  | otherwise = deposit (creditor, amount) $ M.insert to (Right $ (creditor, owed - amount):otherCreditors) accs
  where
    toAccount = accs M.! to
    Left balance = toAccount
    Right creditors = toAccount
    (creditor, owed):otherCreditors = creditors


withdraw :: (String, Int) -> Account -> (Account, Int)
withdraw (to, amount) (Left balance)
  | balance >= amount = (Left $ balance - amount, amount)
  | otherwise = (Right $ [(to, amount - balance)], balance)
withdraw (to, amount) (Right creditors) = (Right $ creditors ++ [(to, amount)], 0)


apply3 :: [(String , String, Int)] -> Map String Account -> Map String Account
apply3 [] bals = bals
apply3 ((from, to, amount):ts) bals = apply3 ts newAccounts
  where
    (newFromAccount, amountWithdrawn) = withdraw (to, amount) $ bals M.! from
    newAccounts = deposit (to, amountWithdrawn) $ M.insert from newFromAccount bals


prob9 :: IO ()
prob9 = do
  ss <- getF lines 9
  let (bals, trans) = parse ss
      
  putStrLn $ "Prob9: part1: " ++ show (sum $ take 3 $ sortOn Down $ M.elems $ apply1 trans bals)
  putStrLn $ "Prob9: part2: " ++ show (sum $ take 3 $ sortOn Down $ M.elems $ apply2 trans bals)
  putStrLn $ "Prob9: part3: " ++ show (sum $ take 3 $ sortOn Down $ lefts $ M.elems $ apply3 trans $ Left <$> bals)

  return ()

