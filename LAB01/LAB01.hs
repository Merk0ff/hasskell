import System.Environment

divisors :: Integer -> [Integer]
divisors k = divisors' 2 k
  where -- (>) $ (*) n n $ k
    divisors' n k | (>) ((*) n n) k = k : []
                  | (==) ((*) n n) k = n : k : []
                  | (==) ((mod) k n) 0 = (n : (k `div` n) : result)
                  | otherwise = result
        where 
            result = divisors' (n + 1) k

main = do
    input <- getArgs
    putStrLn.show $ divisors $ (read $ head input :: Integer)