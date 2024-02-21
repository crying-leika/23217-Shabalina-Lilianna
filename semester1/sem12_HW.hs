type VarName = String
data LambdaTerm
    = Lam VarName LambdaTerm -- Abstraction
    | App LambdaTerm LambdaTerm -- Application
    | Var VarName -- Variable

--1

instance Show LambdaTerm where
    show (Var x) = x
    show (Lam x t) = "(λ " ++ x ++ " . " ++ show t ++ ")"
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

-- как говорится "это немного, но это честная работа"
