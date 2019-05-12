module Functional_Lang where

type Var = String

type Env = Var -> Value

initEnv:: Env
initEnv = const (ValInt 0)

liftInt2:: (Integer -> Integer -> Integer) -> Value -> Value -> Value
liftInt2 op (ValInt i) (ValInt i') = ValInt (i `op` i')
liftInt2 op (ValError s) _ = ValError s

{---------------------------------------------------------------}
data Value = ValInt Integer
          | ValFunc String Exp Env --Primeira Definição
          | ValError String
--    deriving (Show)
instance Show Value where
  show (ValInt i) = show i
  show (ValFunc s e env) = " UNSHOWABLE "--"\\"++s++"."++(show e)
  show (ValError s) = s
{---------------------------------------------------------------}
data Exp = ExpK Integer
        | ExpAdd Exp Exp
        | ExpMul Exp Exp
        | ExpVar Var
        | ExpApp Exp Exp
        | ExpLambda String Exp --Primeira Definição
--    deriving (Show)
instance Show Exp where
  show (ExpK i) = show i
  show (ExpAdd e e') = " ("++(show e)++" + "++(show e')++") "
  show (ExpMul e e') = " ("++(show e)++" * "++(show e')++") "
  show (ExpVar v) = show v
  show (ExpApp e e') = " ("++(show e)++") $ ("++(show e')++") "
  show (ExpLambda s e) = "\\"++s++"->"++(show e)

evalExp:: Exp -> Env -> Value
evalExp (ExpK n) env = ValInt n
evalExp (ExpAdd e e') env = liftInt2 (+) (evalExp e env) (evalExp e' env)
evalExp (ExpMul e e') env = liftInt2 (*) (evalExp e env) (evalExp e' env)
evalExp (ExpVar e) env = env e
evalExp (ExpLambda v b) env = ValFunc v b env --Primeira Definição
evalExp (ExpApp e e') env = --Primeira Definição
    case (evalExp e env) of
        ValFunc v b env' ->
          evalExp b (extend env' v (evalExp e' env))
        ValError s ->
          ValError s

{-------------------------------------------------------------}
extend:: Env -> Var -> Value -> Env
extend env var val = \x -> if x == var then val else env x

{-------------------------------------------------------------}
