{-
  Dukshtau Philip
  15.12.2019
-}


import Control.Monad (liftM2)

{- 
  Lab 01 
    divisors: return divisors of num
    isPerfect: return is number perfect
    listOfPerfect: return list of perfect numbers
-}

divisors :: Integer -> [Integer]
divisors a = [ x | x <- [1 .. a], a `mod` x == 0 ]

isPerfect :: Integer -> Bool
isPerfect a
  | a < 2 = False
  | otherwise = sum (divisors a) `div` 2 == a

listOfPerfect :: Integer -> [Integer]
listOfPerfect a = [x | x <- [1 .. a], isPerfect x == True]

{- 
  Lab 02 
    splitList: split list by possition
    splitBy: split list by condition
    splitWords: generate kist of words
-}

splitList :: Int -> [a] -> ([a], [a])
splitList n list = (take n list , drop n list)

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy condition list = (takeWhile condition list, dropWhile condition list)

splitWords :: String -> [String]
splitWords [] = []
splitWords xxs@(x : xs) 
  | x == ' '  = splitWords xs
  | otherwise = ys : splitWords rest
    where (ys, rest) = break (== ' ') xxs

{- 
  Lab 03
    Expr: data
    instance Show Expr: add new type to show calss
    eval: solve basic expressions
-}

data Expr = 
      Val Double
    | Var String
    | Sum (Expr) (Expr)
    | Minus (Expr) (Expr)
    | Mult (Expr) (Expr)
    | Div (Expr) (Expr)

instance Show Expr where 
  show (Val a) = show(a)
  show (Var a) = show(a)
  show (Sum a b) = show(a) ++ " + " ++ show(b) 
  show (Mult a b) = show(a) ++ " * " ++ show(b)
  show (Minus a b) = show(a) ++ " - " ++ show(b) 
  show (Div a b) = show(a) ++ " / " ++ show(b) 

type Env = [(String, Double)]

eval :: Env -> Expr -> Maybe Double
eval env (Val a) = Just a
eval env (Var a) = lookup a env
eval env (Sum a b) = liftM2 (+) (eval env a) (eval env b)
eval env (Mult a b) = liftM2 (*) (eval env a) (eval env b)
eval env (Minus a b) = liftM2 (-)(eval env a) (eval env b)
eval env (Div a b) = liftM2 (/) (eval env a) (eval env b)
