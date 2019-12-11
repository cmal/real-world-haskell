-- file: ch03/Exercise.hs

import Data.List (sortBy)

myLength :: [] a -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


myAvg :: Fractional a => [a] -> a
myAvg xs = sum xs / (fromIntegral $ length xs)


palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ (tail (reverse xs))

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome (x:xs) = (x == (last xs)) && isPalindrome (take ((length xs) - 1) xs)


comp :: [a] -> [a] -> Ordering
comp a b
  | length a == length b = EQ
  | length a > length b = GT
  | otherwise = LT

sort :: [[a]] -> [[a]]
sort = sortBy comp


-- file: ch03/Intersperse.hs
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse a (x:xs) = x ++ [a] ++ intersperse a xs


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

tree1 =  Node "x" Empty Empty
tree2 = Node "x" Empty (Node "y" Empty Empty)

left :: Tree a -> Tree a
left Empty = Empty
left (Node _ left _) = left
  
right :: Tree a -> Tree a
right Empty = Empty
right (Node _ _ right ) = right

height :: Tree a -> Int
height Empty = 0
height a = 1 + max (height $ left a) (height $ right a)


data Point = Point {
     x :: Int
   , y :: Int
   } deriving (Show)

a = Point 1 3
b = Point 2 4
c = Point 3 6
d = Point 2 7
e = Point 3 5

data Direction = TheLeft | TheRight | ALine deriving (Show)

turn :: Point -> Point -> Point -> Direction
turn a b c
  | res > 0 = TheLeft
  | res == 0 = ALine
  | res < 0 = TheRight
  where res = toLeft a b c


-- 这个 toLeft 测试是计算几何里的 toLeft test
-- 不是这里应该用的 toLeft
-- 这里要计算的是转向
toLeft :: Point -> Point -> Point -> Int
toLeft a b c = (x b) * (y c) - (x c) * (y b)
               - (x a) * (y c) + (x c) * (y a)
               + (x a) * (y b) - (x b) * (y a)


turns :: [Point] -> [Direction]
turns [] = []
turns (x:[]) = []
turns (x:y:[]) = []
turns (x:y:z:xs) = [turn x y z] ++ turns (y:z:xs)


-- Graham Scan for convex hull
