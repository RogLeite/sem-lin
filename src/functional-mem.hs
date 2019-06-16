--Funcional com Efeitos Colaterais usando monads
type Var = String

type Location = Int

type Env = Var -> Location -- Todas as variáveis são boxed agora
emptyEnv:: Env
emptyEnv = const 0
updateEnv:: Var -> Location -> Env -> Env
updateEnv var l env = \x -> if x == var then l else env x


data Value = ValInt Integer
            | ValFunc (Value -> Cmpt Value)
            | ValError String

instance Show Value where
  show (ValInt i) = show i
  show (ValFunc f) = " FUNCTION "--"\\"++s++"."++(show e)
  show (ValError s) = s

type Mem = Location -> Value

freeMem:: Cmpt Location
freeMem m =
       let fr = freeLoc 0 in (fr, m)
      where
        freeLoc i = if isFree(m i) then i else freeLoc (i+1)
        isFree (ValError _) = True
        isFree (_) = False

bindMem:: Var -> Value -> Env -> Cmpt Env
bindMem var val env =
  freeMem >>= (\l -> (updateMem l val) >>= ( \_-> return (updateEnv var l env) ) )

updateMem:: Location -> Value -> Cmpt ()
updateMem loc val m = ((),(update loc val m))

--Update Genérico
update:: Eq a => a -> b -> (a -> b) -> (a -> b)
update k val find = \x -> if x == k then val else find x



newtype Cmpt a = Cmpt (Mem -> ( a , Mem))
instance Monad (Cmpt b) where
  return a = \m -> (a, m)
  (>>=) (Cmpt ca) f = \m -> let (a, m') = ca m in --
                      f a m'
                      -- 080
-- executes a binary operation on computations                            -- 081
op2 :: (a -> b -> Cmpt c) -> Cmpt a -> Cmpt b -> Cmpt c                   -- 082
op2 op ca cb = ca >>= (\a -> cb >>= (op a))
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

closure:: Var -> Exp -> Env -> Value
closure var ex env = ValFunc f
  where f x = (bindMem var x) >>= (evalExp ex) -- É igual a \env' -> evalExp ex env'

evalExp:: Exp -> Env -> Cmpt Value
evalExp (ExpK n) env = return (ValInt n)
evalExp (ExpAdd e e') env = op2 (+) (evalExp e env) (evalExp e' env)
evalExp (ExpMul e e') env = op2 (*) (evalExp e env) (evalExp e' env)
evalExp (ExpVar e) env = return (env e)
evalExp (ExpLambda v e) env = return (closure v (evalExp e) env) --Segunda Definição
evalExp (ExpApp e e') env = op2 app (evalExp e env) (evalExp e' env)--Segunda Definição
            where app (ValFunc f) v = f v
