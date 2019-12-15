import System.Environment
import Control.Monad (liftM2 )

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

main = do
    -- input <- getArgs
    putStrLn.show $ eval [(1, x)] (Val 3)
