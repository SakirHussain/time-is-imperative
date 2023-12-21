f :: Num a => a -> a
f x = x + 2
-- ghci> :t f
-- f :: Num a => a -> a
-- ghci> f 3 == 5
-- True

-- guards
absolute :: (Ord a, Num a) => a -> a
absolute x
    | x < 0     = -1 * x -- -x also possible
    | otherwise = x

numOfRealSolutions :: (Num a1, Num a2, Ord a2) => a2 -> a2 -> a2 -> a1
numOfRealSolutions a b c
    | disc > 0  = 2
    | disc == 0 = 1
    | otherwise = 0
        where
        disc = b^2 - 4*a*c