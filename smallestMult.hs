
multsOf n = [x | x <- [n-1, n-2..2], n `rem` x == 0]


primes n = go [] [2..n]
  where go acc (x:xs) = go (acc ++ [x]) $ filter (\y -> y `rem` x /= 0) xs
        go acc [] = acc

filterlistBy :: Eq a => [a] -> [a] -> [a]
filterlistBy a [] = a
filterlistBy [] _ = []
filterlistBy a b = go a b
  where go xs (y:ys) = go (del y xs) ys
        go xs [] = xs


del :: Eq a => a -> [a] -> [a]
del x s = go s []
  where go [] prev = prev
        go (y:ys) prev
          | x == y = prev ++ ys
          | otherwise = go ys (prev ++ [y])

-- if number is divisible by 20 then it is divisible by 5
gdivs :: Int -> [Int]
gdivs n = go (primes n) []
  where go (x:xs) acc = go xs (acc ++ checkDup (x `gdiv` n) acc)
          where 
                checkDup x [] = [x]
                checkDup x (y:ys)
                  | x == y = []
                  | otherwise = checkDup x ys
        go [] acc = acc

gdiv :: Int -> Int -> Int
gdiv x n = head $ [x*y | y <- [n,n-1..1], x*y <= n]


