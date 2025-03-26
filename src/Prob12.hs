module Prob12(prob12) where

import Utils 


prob12 :: IO ()
prob12 = do
  ss <- getLines 12
      
  putStrLn $ "Prob12: part1: " ++ show ( ss)

  return ()

