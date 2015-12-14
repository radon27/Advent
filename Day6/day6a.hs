import Data.List.Split
import Data.List

-- Type synonyms ----------------------------------------------------
type Point = (Int,Int)
type Rectangle = (Point,Point)
type IndexValue = (Point,Bool)
type LightArray = [IndexValue]

-- List definitions -------------------------------------------------

-- allPoints defines a list of all Points that are within 
-- a 1000 x 1000 matrix
allPoints :: [Point]
allPoints = [(a,b) | a <- [0..999], b <- [0..999]]

-- allOffB defines a 1000 x 1000 matrix
-- with every element equal to False 
allOffB :: [Bool]
allOffB = take 1000000 . repeat $ False

-- allOffIV defines a 1000 x 1000 matrix of pairs
-- The first part of the pair is a Point in the matrix 
-- and the second part is equal to False
allOffIV :: LightArray
allOffIV = zip allPoints allOffB 

-- allOnB defines a 1000 x 1000 matrix
-- with every element equal to True
allOnB :: [Bool]
allOnB = take 1000000 . repeat $ True

-- allOnIV defines a 1000 x 1000 matrix of pairs
-- The first part of the pair is a Point in the matrix 
-- and the second part is equal to True
allOnIV :: LightArray
allOnIV = zip allPoints allOnB

-- Function definitions ---------------------------------------------

-- turnOn takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle are now turned on
turnOn :: LightArray -> Rectangle -> LightArray
turnOn [] _ = []
turnOn ((p,b):rest) r
  | isInRectangle p r = (p,True) : turnOn rest r
  | otherwise = (p,b) : turnOn rest r

-- turnOff takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle are now turned off
turnOff :: LightArray -> Rectangle -> LightArray
turnOff [] _ = [] 
turnOff ((p,b):rest) r 
  | isInRectangle p r = (p,False) : turnOff rest r 
  | otherwise = (p,b) : turnOff rest r 

-- toggle takes a LightArray and a Rectangle
-- and returns a new LightArray that is a copy of the first
-- except all lights within the Rectangle are now in the opposite state
-- That is, lights that were on are now off
-- and lights that were off are now on
toggle :: LightArray -> Rectangle -> LightArray
toggle [] _ = []
toggle ((p,b):rest) r 
  | isInRectangle p r = (p,not b) : toggle rest r 
  | otherwise = (p,b) : toggle rest r   

-- isInRectangle takes a Point and a Rectangle
-- If the Point is within the Rectangle, True is returned
-- If the Point is outside the Rectangle, False is returned
isInRectangle :: Point -> Rectangle -> Bool
isInRectangle (x,y) ((x1,y1),(x2,y2))
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = True
  | otherwise = False
  
-- count takes a LightArray and returns the number of lights that are on
count :: LightArray -> Int 
count = length . filter snd

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

-- A small data set to test functionality ---------------------------

testTurnOn :: String
testTurnOn = "turn on 943,30 through 990,907"

testTurnOff :: String 
testTurnOff = "turn off 674,321 through 793,388"

testToggle :: String 
testToggle = "toggle 749,672 through 973,965"

test :: String
test = "turn off 674,321 through 793,388\ntoggle 749,672 through 973,965\nturn on 943,30 through 990,907"

-- main -------------------------------------------------------------

main :: IO ()
main = do 
--  let input = test
--  let input = testTurnOn
--  let input = testTurnOff
--  let input = testToggle
  input <- readFile "day6a.txt"
  let temp = splitOn "\n" input
--  let output = count . foldl dispatch allOnIV $ temp 
  let output = count . foldl dispatch allOffIV $ temp
  print $ output 
  

