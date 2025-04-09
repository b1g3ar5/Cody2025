module Prob16_2(prob16) where
import Utils hiding (lt, rt, up, dn, scale, Coord)
import Data.Map qualified as M
import Data.Array qualified as A
import Linear.V3
import Linear.Vector
import Linear.Matrix
import Data.List (intersect)

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
type Coord = V3 Int
type Face = A.Array Coord Int
type Cube = M.Map Coord Face
type Observer = (Coord, Coord)
type State = (Observer, Cube)

-- Rotation matrices

clockx, clocky, clockz, antix, antiy, antiz :: M33 Int
clockx = V3 (V3 1   0   0)  (V3   0  0 (-1)) (V3   0   1  0) 
clocky = V3 (V3 0   0   1)  (V3   0  1   0)  (V3 (-1)  0  0) 
clockz = V3 (V3 0 (-1)  0)  (V3   1  0   0)  (V3   0   0  1) 
antix =  V3 (V3 1   0   0)  (V3   0  1   0)  (V3   0 (-1) 0) 
antiy =  V3 (V3 0   0 (-1)) (V3   0  1   0)  (V3   1   0  0) 
antiz =  V3 (V3 0   1   0)  (V3 (-1) 0   0)  (V3   0   0  1) 


twist :: Char -> Observer -> Observer
twist 'L' = bimap (clocky !*) (clocky !*)
twist 'R' = bimap (antiy !*) (antiy !*)
twist 'U' = bimap (clockx !*) (clockx !*)
twist 'D' = bimap (antix !*) (antix !*)
twist c   = error $ "Unknown twist: " ++ [c]


add :: Int -> Int -> Int
add x y = 1 + (x+y-1) `mod` 100

cubeSize :: Int
cubeSize = 80

lo, hi :: Int
lo = -(cubeSize - 1)
hi = (cubeSize - 1)

lohi :: [Int]
lohi = [lo, (lo+2)..hi]

unitScale :: Int
unitScale = (cubeSize `div` 2) - 1

makeCube :: Cube
makeCube = M.fromList $ [ (V3 (-1) 0 0, A.array (V3 lo lo lo, V3 lo hi hi) [(V3 lo y z, 1) | y<-lohi, z<-lohi])
                        , (V3  1 0 0,   A.array (V3 hi lo lo, V3 hi hi hi) [(V3 hi y z, 1) | y<-lohi, z<-lohi])
                        , (V3 0 (-1) 0, A.array (V3 lo lo lo, V3 hi lo hi) [(V3 x lo z, 1) | z<-lohi, x<-lohi])
                        , (V3 0 1 0,    A.array (V3 lo hi lo, V3 hi hi hi) [(V3 x hi z, 1) | z<-lohi, x<-lohi])
                        , (V3 0 0 (-1), A.array (V3 lo lo lo, V3 hi hi lo) [(V3 x y lo, 1) | y<-lohi, x<-lohi])
                        , (V3 0 0  1,   A.array (V3 lo lo hi, V3 hi hi hi) [(V3 x y hi, 1) | y<-lohi, x<-lohi])]


getFace :: Observer -> [V3 Int]
getFace (pos, dir) = [pos + (2*unitScale) *^ pos + ix *^ xunit + iy *^ yunit | ix <- lohi, iy <- lohi]
  where
    yunit = dir
    xunit = pos `cross` dir


getCol :: Int -> Observer -> [V3 Int]
getCol n (pos, dir) = [pos + (2*unitScale) *^ pos + cix *^ yunit + (lo + 2 * fromIntegral (n-1)) *^ xunit | cix <- lohi]
  where
    yunit = dir
    xunit = pos `cross` dir


getRow :: Int -> Observer -> [V3 Int]
getRow n (pos, dir) = [pos + (2*unitScale) *^ pos + (lo + 2 * fromIntegral (n-1)) *^ yunit + rix *^ xunit | rix <- lohi]
  where
    yunit = dir
    xunit = pos `cross` dir


apply1 :: Instruction -> State -> State
apply1 ins (obs@(pos, _), cube) = (obs, go ins)
  where 
    go :: Instruction -> Cube
    go (Face,inc) = M.adjust ((+ inc) <$>) pos cube 
    go (Row rix, inc) = M.adjust (\arr -> A.accum (+) arr [(i, inc) | i <- cs]) pos cube 
      where
        cs = getRow rix obs
    go (Col rix, inc) = M.adjust (\arr -> A.accum (+) arr [(i, inc) | i <- cs]) pos cube 
      where
        cs = getCol rix obs



process1 :: (Char -> Observer -> Observer) -> [Instruction] -> String -> State -> State
process1 _ [] _ cube = cube
process1 _ (i:_) [] cube = apply1 i cube
process1 tw (i:is) (t:ts) cube = process1 tw is ts $ first (tw t) $ apply1 i cube


-- Just the product of the 2 faces with biggest sums
score :: Cube -> [Int]
score cube = sum . (cube M.! ) <$> faces

safeLookup :: (Show k, Ord k) => M.Map k a -> k -> a
safeLookup mp x
  | x `M.member` mp = mp M.! x
  | otherwise = error $ "Lookup error: " ++ show x


faces, scaledFaces :: [V3 Int]
faces = [V3 0 0 (-1), V3 0 0 1, V3 0 (-1) 0, V3 0 1 0, V3 (-1) 0 0, V3 1 0 0]
scaledFaces = [V3 0 0 (-40), V3 0 0 39, V3 0 (-40) 0, V3 0 39 0, V3 (-40) 0 0, V3 39 0 0]


dummyOrientation :: V3 Int -> Observer
dummyOrientation v = (v, w)
  where
    w
      | v == V3 1 0 0 = V3 0 1 0
      | v == V3 0 1 0 = V3 0 0 1
      | v == V3 0 0 1 = V3 1 0 0
      | v == V3 (-1) 0 0 = V3 0 (-1) 0
      | v == V3 0 (-1) 0 = V3 0 0 (-1)
      | v == V3 0 0 (-1) = V3 (-1) 0 0
      | otherwise = error $ "Not a basis V3: " ++ show v



prob16 :: IO ()
prob16 = do
  ss <- getF lines 16
  let (instructions, twists) = parse ss
      c0 = makeCube
      obs :: Observer
      obs@(pos, dir) = (V3 0 0 (-1), V3 0 (-1) 0)
      finalCube1 = process1 twist instructions twists (obs, c0)
      s0 = (obs, c0)
      (o1, c1) = foldr apply1 s0 $ take 1 instructions
      s1 = process1 twist (take 100 instructions) twists s0
      s2 = process1 twist (take 200 instructions) twists s0


  --putStrLn $ "Prob1: part1:score c1" ++ show (score c1)
  --putStrLn $ "Prob1: part1:score c1: " ++ show (length $ A.range $ A.bounds $ c1 M.! (V3 1 0 0))
  putStrLn $ "Prob1: part1:score s1: " ++ show (getRow 1 obs)
  --putStrLn $ "Prob1: part1:score s2" ++ show (second score s2)
  --putStrLn $ "Prob1: part1: obs " ++ show (obs)
  --putStrLn $ "Prob1: part1: L obs " ++ show (twist 'L' obs)
  --putStrLn $ "Prob1: part1: U L obs " ++ show (twist 'U' $ twist 'L' obs)
  --putStrLn $ "Prob1: part1: R U L obs " ++ show (twist 'R' $ twist 'U' $ twist 'L' obs)
  --putStrLn $ "Prob1: part1: D R U L obs = z y" ++ show (twist 'D' $ twist 'R' $ twist 'U' $ twist 'L' obs)

