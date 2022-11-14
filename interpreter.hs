
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List


data Expr = Lit Int
          | Var String
          | Expr :+ Expr
          | Expr :* Expr
          | If Expr Expr Expr
          deriving (Show)

data Assignment = String := Expr deriving (Show)

-- used a map to store values of variables
type VarMap = Map String Int

-- eval func evaluates individual expression based on var lookup
eval :: VarMap -> Expr -> Expr
eval m (Lit a) = Lit a
eval m (Var a) = case (Map.lookup a m) of 
                 Just v -> Lit v
                 Nothing -> Var a
eval m (a :+ b) = case ((eval m a), (eval m b)) of
                  ((Lit x), (Lit y)) -> Lit (x + y)
                  (_,_) -> (eval m a) :+ (eval m b)
eval m (a :* b) = case ((eval m a), (eval m b)) of
                  ((Lit x), (Lit y)) -> Lit (x * y)
                  (_,_) -> (eval m a) :* (eval m b)
eval m (If a b c) = case (eval m a) of
                    Lit x -> if x > 0 then (eval m b) else (eval m c)
                    Var x -> If a (eval m b) (eval m c)
                

-- Optimise func uses recursion to evaluate list of subexpressions
optimise :: [Assignment] -> [Assignment]
optimise xs = aux Map.empty xs
  where
    aux m [] = [] 
    aux m ((str := expr):xs) = case (eval m expr) of
                              Lit x -> (str := (Lit x)):(aux (Map.insert str x m) xs)
                              (_) -> (str := (eval m expr)):(aux m xs)
                    
{-Test
let ls = ["x" := ((Lit 1 :+ Lit 2) :* Lit 3),"y" := (Var "x" :+ (Lit 4 :* Lit 5)),"x" := (Var "y" :+ Lit (-30)),"z" := (Var "z" :+ (If (Var "x") (Lit 6) (Var "v"))),"t" := (Lit 1 :+ (Lit 2 :+ Var "u"))]
optimise ls
["x" := Lit 9,"y" := Lit 29,"x" := Lit (-1),"z" := (Var "z" :+ Var "v"),"t" := (Lit 1 :+ (Lit 2 :+ Var "u"))]
-}
