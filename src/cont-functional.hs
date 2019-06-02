module Cont_Functional_Lang where

type Var = String
type Result = String
type Env = Var -> Value

initEnv:: Env
initEnv = const (ValInt 0)

liftInt2:: (Integer -> Integer -> Integer) -> Value -> Value -> Value
liftInt2 op (ValInt i) (ValInt i') = ValInt (i `op` i')
liftInt2 op (ValError s) _ = ValError s

{---------------------------------------------------------------}
type K a = a -> Result
type Cmpt a = K a -> Result

op0:: a -> Cmpt a -- a -> K a -> Result
op0 a k = k a
{--
op1:: (a -> b) -> Cmpt a -> Cmpt b --op1:: (a -> b) -> Cmpt a -> K b -> Result
op1 f cpa kb = cpa (\a -> kb (f a))
--}
op1:: Cmpt a -> (a -> Cmpt b) -> Cmpt b -- op1:: Cmpt a -> (a -> Cmpt b) -> K b -> Result
op1 ca f kb = ca (\a -> f a kb)

op2::(a -> b -> Cmpt c) -> Cmpt a -> Cmpt b -> Cmpt c
op2 op ca cb = op1 ca (\a-> op1 cb (\b-> a `op` b))

{---------------------------------------------------------------}
data Value = ValInt Integer
          | ValFunc (Value -> Cmpt Value) --Primeira Definição
          | ValError String
--    deriving (Show)
instance Show Value where
  show (ValInt i) = show i
  show (ValFunc f) = " FUNCTION "--"\\"++s++"."++(show e)
  show (ValError s) = s
{---------------------------------------------------------------}
data Exp = ExpK Integer
        | ExpAdd Exp Exp
        | ExpMul Exp Exp
        | ExpVar Var
        | ExpApp Exp Exp
        | ExpLambda String Exp --Primeira Definição
        | ExpNeg Exp
--    deriving (Show)
instance Show Exp where
  show (ExpK i) = show i
  show (ExpAdd e e') = " ("++(show e)++" + "++(show e')++") "
  show (ExpMul e e') = " ("++(show e)++" * "++(show e')++") "
  show (ExpVar v) = show v
  show (ExpApp e e') = " ("++(show e)++") $ ("++(show e')++") "
  show (ExpLambda s e) = "\\"++s++"->"++(show e)
  show (ExpNeg n) = " (- "++(show n)++" ) "

evalExp:: Exp -> Env -> Cmpt Value
evalExp (ExpK n) env = op0 (ValInt n)
evalExp (ExpAdd e e') env = op2 (liftInt2 (+)) (evalExp e env) (evalExp e' env)
--evalExp (ExpAdd e e') env = (liftInt2 (+) (evalExp e env) (evalExp e' env))
evalExp (ExpMul e e') env = op2 (liftInt2 (*)) (evalExp e env) (evalExp e' env)
--evalExp (ExpMul e e') env = (liftInt2 (*) (evalExp e env) (evalExp e' env))
evalExp (ExpVar e) env = op0 (env e)
evalExp (ExpLambda v e) env = op0 (closure v (evalExp e) env) --Segunda Definição
evalExp (ExpApp e e') env = op2 app (evalExp e env) (evalExp e' env)--Segunda Definição
            where app (ValFunc f) v = f v
evalExp (ExpNeg n) env = op1 neg (evalExp n env)
            where neg (ValInt i) = ValInt (- i) --neg:: Value -> Value

{-------------------------------------------------------------}
bind:: Env -> Var -> Value -> Env
bind env var val = \x -> if x == var then val else env x

closure:: Var -> (Env -> Cmpt Value) -> Env -> Value
closure v e env = ValFunc (\x -> e (bind v x env))

{-------------------------------------------------------------}
