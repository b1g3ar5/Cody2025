module Prob16(prob16) where

import Utils hiding (lt, rt, up, dn)
import Data.Array qualified as A

parse :: [String] -> ([Instruction], String)
parse ss = (parseInstruction <$> ps!!0, head $ ps!!1)
  where
    ps = splitOn "" ss


parseInstruction :: String -> Instruction
parseInstruction l
  | ws!!0 == "FACE" = (Face, read $ ws!!3)
  | ws!!0 == "ROW" = (Row $ read $ ws!!1, read $ ws!!4)
  | ws!!0 == "COL" = (Col $ read $ ws!!1, read $ ws!!4)
  | otherwise = error $ "Unknown target: " ++ show l
  where
    ws = words l


data Target = Face | Row Int | Col Int deriving (Eq, Show)
type Instruction = (Target, Int)
type Face = [[Int]]
data Cube = Cube { ft::Face, bk::Face, lt::Face, rt::Face, up::Face, dn::Face} deriving (Eq, Show)


add :: Int -> Int -> Int
add x y = 1 + ((x+y-1) `mod` 100)

-- Apply to the front face only
apply1 :: Instruction -> Cube -> Cube
apply1 ins cube = cube {ft = go ins}
  where 
    go (Face,v) = ((`add` v) <$>) <$> ft cube
    go (Row rix, v) = zipWith (\rw ix -> if ix==rix then (`add` v)<$>rw else rw) (ft cube) [1..]
    go (Col cix, v) = (zipWith (\ix x -> if ix==cix then x `add` v else x) [1..]) <$> (ft cube)
    

-- Apply Row and Col round the cube
apply3 :: Instruction -> Cube -> Cube
apply3 ins (Cube f b l r u d) = Cube (go ins f) (gob ins) (gol ins) (gor ins) (gou ins) (god ins)
  where 
    n = length f

    go :: Instruction -> Face -> Face
    go (Face,v) dat = ((`add` v) <$>) <$> dat
    go (Row rix, v) dat = zipWith (\ix rw -> if ix==rix then (`add` v)<$>rw else rw) [1..] dat
    go (Col cix, v) dat = (zipWith (\ix x -> if ix==cix then x `add` v else x) [1..]) <$> dat
    
    gob (Face, _) = b
    gob i@(Row _, _) = go i b
    gob (Col cix, v) = go (Col (n+1-cix), v) b
    
    gol (Face, _) = l
    gol i@(Row _, _) = go i l
    gol (Col _, _) = l

    gor (Face, _) = r
    gor i@(Row _, _) = go i r
    gor (Col _, _) = r

    gou (Face, _) = u
    gou (Row _, _) = u
    gou i@(Col _, _) = go i u

    god (Face, _) = d
    god (Row _, _) = d
    god i@(Col _, _) = go i d


clock, anti :: [[a]] -> [[a]]
clock = (reverse <$>) . transpose
anti  = (reverse ) . transpose


-- Faces ft, lt, bk, rt are oriented upwards in the y axis
-- Face top of fac up is at +z end, top of face dn is at -z end
twist :: Char -> Cube -> Cube
-- when L cones to the front the top turns anti and the bottom clock
twist 'L' inCube = Cube (lt inCube) (rt inCube) (bk inCube) (ft inCube) (anti $ up inCube) (clock $ dn inCube)
twist 'R' inCube = Cube (rt inCube) (lt inCube) (ft inCube) (bk inCube) (clock $ up inCube) (anti $ dn inCube)
-- when the up fac comes to the front the down face goes to the back - but the top left corner needs to go diagonally
-- similarly the back becomes the top but the top left corner is diagonally wrong
twist 'U' inCube = Cube (up inCube) (reverse . (reverse <$>) $ dn inCube) (clock $ lt inCube) (anti $ rt inCube) (reverse . (reverse <$>) $ bk inCube) (ft inCube)
twist 'D' inCube = Cube (dn inCube) (reverse . (reverse <$>) $ up inCube) (anti $ lt inCube) (clock $ rt inCube) (ft inCube) (reverse . (reverse <$>) $ bk inCube)
twist c _ = error $ "Unknown move: " ++ [c]


makeCube :: Int -> Cube
makeCube n = Cube f f f f f f
  where
    f :: [[Int]]
    f = replicate n $ replicate n 1


process1 :: (Char -> Cube -> Cube) -> [Instruction] -> String -> Cube -> Cube
process1 _ [] _ cube = cube
process1 _ (i:_) [] cube = apply1 i cube
process1 tw (i:is) (t:ts) cube = process1 tw is ts $ tw t $ apply1 i cube

process3 :: (Char -> Cube -> Cube) -> [Instruction] -> String -> Cube -> Cube
process3 _ [] _ cube = cube
process3 _ (i:_) [] cube = apply3 i cube
process3 tw (i:is) (t:ts) cube = process3 tw is ts $ tw t $ apply3 i cube


score :: Int -> Cube -> Int
score n (Cube f b l r u d) = product $ (\v -> v - n*n) <$> take 2 (sortOn Down $ sum . (sum <$>) <$> [f,b,l,r,u,d])


dominantSum :: Cube -> Integer
dominantSum (Cube f b l r u d) = product $ fromIntegral . ds <$> [f,b,l,r,u,d]


ds :: Face -> Int
ds xss = maximum $ (sum <$> xss) ++ (sum <$> transpose xss)


prob16 :: IO ()
prob16 = do
  ss <- getF lines 16
  let (instructions, twists) = parse ss
      cubeSize = 80
      cube = makeCube cubeSize
      finalCube1 = process1 twist instructions twists cube
      finalCube3 = process3 twist instructions twists cube

  putStrLn $ "Prob16: part1: " ++ show (score cubeSize finalCube1 ) --195459600691200 - mode in add function breaks this
  putStrLn $ "Prob16: part2: " ++ show (dominantSum finalCube1 )
  putStrLn $ "Prob16: part3: " ++ show (dominantSum finalCube3 ) 

