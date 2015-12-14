import Data.List.Split
import Data.List

-- Type synonyms ----------------------------------------------------
type Point = (Int,Int)
type Rectangle = (Point,Point)
type IndexValue = (Point,Int)
type LightArray = [IndexValue]

-- List definitions -------------------------------------------------

-- allPoints defines a list of all Points that are within 
-- a 1000 x 1000 matrix
allPoints :: [Point]
allPoints = [(a,b) | a <- [0..999], b <- [0..999]]

-- allOffB defines a 1000 x 1000 matrix
-- with every element equal to 0
allOffB :: [Int]
allOffB = take 1000000 . repeat $ 0

-- allOffIV defines a 1000 x 1000 matrix of pairs
-- The first part of the pair is a Point in the matrix 
-- and the second part is equal to 0
allOffIV :: LightArray
allOffIV = zip allPoints allOffB 

-- Function definitions ---------------------------------------------

-- turnOn takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle now have a brightness of +1
turnOn :: LightArray -> Rectangle -> LightArray
turnOn [] _ = []
turnOn ((p,b):rest) r
  | isInRectangle p r = (p,b + 1) : turnOn rest r
  | otherwise = (p,b) : turnOn rest r

-- turnOff takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle now have a brightness of -1 
-- with a minimum brightness of 0 
turnOff :: LightArray -> Rectangle -> LightArray
turnOff [] _ = [] 
turnOff ((p,b):rest) r 
  | b > 0 && isInRectangle p r = (p,b - 1) : turnOff rest r 
  | otherwise = (p,b) : turnOff rest r 

-- toggle takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle now have a brightness of +2
toggle :: LightArray -> Rectangle -> LightArray
toggle [] _ = []
toggle ((p,b):rest) r 
  | isInRectangle p r = (p,b + 2) : toggle rest r 
  | otherwise = (p,b) : toggle rest r   

-- isInRectangle takes a Point and a Rectangle
-- If the Point is within the Rectangle, True is returned
-- If the Point is outside the Rectangle, False is returned
isInRectangle :: Point -> Rectangle -> Bool
isInRectangle (x,y) ((x1,y1),(x2,y2))
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False
  
-- count takes a LightArray and returns the combined brightness of all lights 
count :: LightArray -> Int 
count = sum . map snd

-- dispatch takes a LightArray and a dirty String
-- It decides which function to call
-- and calls that function with the LightArray and deciphered Rectangle
-- A new LightArray is returned
dispatch :: LightArray -> String -> LightArray
dispatch la s 
  | isPrefixOf "turn on " s = turnOn la (getRectangle . drop 8 $ s)
  | isPrefixOf "turn off " s = turnOff la (getRectangle . drop 9 $ s)
  | isPrefixOf "toggle " s = toggle la (getRectangle . drop 7 $ s)
  | otherwise = la 

-- getRectangle takes a String and returns a Rectangle 
-- It is assumed that the String has the format
-- "[0-9]+,[0-9]+ through [0-9]+,[0-9]+"
getRectangle :: String -> Rectangle
getRectangle s = (p1,p2)
  where [ul,lr] = splitOn " through " s 
        p1 = getPoint ul 
        p2 = getPoint lr

-- getPoint takes a String and returns a Point 
-- It is assumed that the String has the format
-- "[0-9]+,[0-9]+"
getPoint :: String -> Point 
getPoint s = (x,y)
  where [x,y] = fmap read (splitOn "," s)  

-- main -------------------------------------------------------------

main :: IO ()
main = do 
  input <- readFile "day6a.txt"
  let temp = splitOn "\n" input
  let output = count . foldl dispatch allOffIV $ temp
  print $ output 
  
