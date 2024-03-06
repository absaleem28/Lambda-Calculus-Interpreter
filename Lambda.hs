module Lambda where

import Expr
import Data.List
import Foreign (free)

-- TODO 1.1. find free variables of a Expr
freeVarsAux :: [String] -> Expr -> [String]
freeVarsAux varsBounded (Variable var) = [var | var `notElem` varsBounded]
freeVarsAux varsBounded (Function var body) = freeVarsAux (var : varsBounded) body
freeVarsAux varsBounded (Application e1 e2) = nub (freeVarsAux varsBounded e1 ++ freeVarsAux varsBounded e2)

free_vars :: Expr -> [String]
free_vars = freeVarsAux []

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable var) replaceVar param 
    | replaceVar == var = param 
    | otherwise = Variable var

reduce (Function var body) replaceVar param
    | var == replaceVar = Function var body
    | var `elem` free_vars param = Function (var ++ "i") (reduce (reduce body var (Variable (var ++ "i"))) replaceVar param)
    | otherwise = Function var (reduce body replaceVar param)

reduce (Application e1 e2) replaceVar param = Application (reduce e1 replaceVar param) (reduce e2 replaceVar param)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable var) = Variable var
stepN (Application (Function var body) expr) = reduce body var expr
stepN (Application e1 e2) = Application (stepN e1) e2
    

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN (Variable var) = Variable var
reduceN (Function var body) = Function var (reduceN body)
reduceN (Application (Variable var) expr) = Application (Variable var) (reduceN expr)
reduceN (Application e1 e2) = reduceN (stepN (Application e1 e2))


reduceAllN :: Expr -> [Expr]
reduceAllN (Variable var) = [Variable var]
reduceAllN (Function var body) = Function var body : reduceAllN body
reduceAllN (Application (Variable x) expr) = Application (Variable x) expr : reduceAllN expr
reduceAllN (Application e1 e2) = Application e1 e2 : reduceAllN (stepN (Application e1 e2))

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable var) = Variable var
stepA (Application (Application e1 e2) e) = Application (stepA (Application e1 e2)) e
stepA (Application e (Application e1 e2)) = Application e (stepA (Application e1 e2))
stepA (Application (Function var body) e) = reduce body var e

reduceA :: Expr -> Expr
reduceA (Variable var) = Variable var
reduceA (Function var body) = Function var (reduceA body)
reduceA (Application (Variable var) expr) = Application (Variable var) (reduceA expr)
reduceA (Application e1 e2) = reduceA (stepA (Application e1 e2))

reduceAllA :: Expr -> [Expr]
reduceAllA (Variable var) = [Variable var]
reduceAllA (Function var body) = Function var body : reduceAllA body
reduceAllA (Application (Variable x) expr) = Application (Variable x) expr : reduceAllA expr
reduceAllA (Application e1 e2) = Application e1 e2 : reduceAllA (stepA (Application e1 e2))

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros env (Macro m) = case lookup m env of
                            (Just mv) -> evalMacros env mv
                            Nothing -> Macro m
evalMacros env (Function var body) = Function var (evalMacros env body)
evalMacros env (Application e1 e2) = Application (evalMacros env e1) (evalMacros env e2)
evalMacros env m = m

-- TODO 4.1. evaluate code sequence using given strategy
evalCodeMacros :: [(String, Expr)] -> (Expr -> Expr) -> [Code] -> [Expr]
evalCodeMacros macrosDic strategy [Assign s e] = []    -- case if assign is the last one in the code list
evalCodeMacros macrosDic strategy [Evaluate e] = [strategy (evalMacros macrosDic e)]
evalCodeMacros macrosDic strategy ((Assign s e) : codes) = evalCodeMacros ((s, e) : macrosDic) strategy codes
evalCodeMacros macrosDic strategy ((Evaluate e) : codes) = strategy (evalMacros macrosDic e) : evalCodeMacros macrosDic strategy codes

evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy codeLines = evalCodeMacros [] strategy codeLines
