import Data.List ()
import GHC.Generics (URec(UDouble))

inRange :: Integer -> Integer -> Integer -> Bool

-- method 1
--inRange min max x = True

--method 2
-- inRange min max x = 
--     let lb = x >= min
--         ub = x <= max
--     in
--         lb && ub

--method 3
-- inRange min max x = lb && ub
--     where
--         lb = x >= min
--         ub = x <= max

--method 4
inRange min max x = 
    if lb then ub else False where
        lb = x >= min
        ub = x <= max

-- fac :: (Ord t, Num t) => t -> t
-- fac n
--     | n <= 1 = 1
--     | otherwise = n * fac (n-1)

isZero :: (Eq t, Num t) => t -> Bool
isZero n
    | n == 0 = True
    | otherwise = False

-- tail recursion 
fac :: (Ord t, Num t) => t -> t
fac n  = aux n 1 where
    aux n acc
        | n <= 1 = acc
        | otherwise = aux (n-1) (n*acc)


-- asc :: (Ord t, Num t) => t -> t -> [t]
-- asc n m
--     | m < n = []
--     | m == n = []
--     | m > n = n : asc (n+1) m
                                                    -- essentially same
asc :: (Ord t, Num t) => t -> t -> [t]
asc n m
    | m <= n = [m]
    | m > n = n : asc (n+1) m

-- sum [] = 0
-- sum(x:xs) = x + sum xs

-- list comprehension 
-- [ 3*x | x <- [1 .. 10]]
-- [3*x | x <- [1 .. 10], x `mod` 2==0]
-- [(x,y) | x <- [1,2,3], y <- ['a','b']]


s :: [Int] -> Int
s [] = 0                     -- pattern matching bc we're 
s (x:xs) = x + s xs          -- pattern matching the list

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    | even x = x : evens xs
    | otherwise = evens xs

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x+y | (x,y) <- xs]

-- ex 1 true if element is in given list
ele :: Eq t => t -> [t] -> Bool
ele _ [] = False
ele e (x:xs) = (e == x) || ele e xs

-- ex 2 create a function that removes all duplicates from a list
nu :: Eq a => [a] -> [a]
nu [] = []
nu (x:xs)
    | x `ele` xs = nu xs
    | otherwise = x : nu xs 
    

-- ex3 check if list is in ascending order
isAsc :: Ord a => [a] -> Bool
isAsc [] = False
isAsc [x] = True
isAsc (x:y:ys) = x <= y && isAsc (y:ys)



-- main :: IO()
-- main = do
--     let result = isAsc [1,2,3,4,5,6,7]
--     let result2 =  fac 0
--     putStrLn $ "Result : " ++ show result ++ " Result2 : " ++ show result2

