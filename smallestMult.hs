import System.Environment (getArgs)

primes n = go [] [2..n]
  where go acc (x:xs) = go (acc ++ [x]) $ filter (\y -> y `rem` x /= 0) xs
        go acc [] = acc

-- e.g. if number is divisible by 20 then it is divisible by 5
gdivs :: Int -> [Int]
gdivs n = go (primes n) []
  where go (x:xs) acc = go xs (acc ++ checkDup (x `gdiv` n) acc)
          where 
                checkDup x [] = [x]
                checkDup x (y:ys)
                  | x == y = []
                  | otherwise = checkDup x ys
        go [] acc = acc


gdiv x n = head $ [x*y | y <- [n,n-1..1], x*y <= n]

seqProds n = gdivs n

minProd = product . seqProds

main = (\(x:_) -> print $ minProd (read x :: Int))=<< getArgs


