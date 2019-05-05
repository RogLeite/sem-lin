module Lang where

type Var = String

type Mem = Var -> Integer

initMem:: Mem
initMem = const 0

{---------------------------------------------------------------}
data Exp = ExpAdd Exp Exp
        | ExpMul Exp Exp
        | ExpNum Integer
        | ExpVar Var
    deriving (Show)
        
evalExp:: Exp -> Mem -> Integer
evalExp (ExpNum n) m = n
evalExp (ExpAdd e e') m = (evalExp e m) + (evalExp e' m)
evalExp (ExpMul e e') m = (evalExp e m) * (evalExp e' m)
evalExp (ExpVar e) m = m e
{-------------------------------------------------------------}
update:: Mem -> Var -> Integer -> Mem
update m var val = \x -> if x == var then val else m x

{-------------------------------------------------------------}

data Cmd = CmdAsg Var Exp
         | CmdIf Exp Cmd Cmd
         | CmdSeq Cmd Cmd
         | CmdSkip          --Elemento Neutro de CmdSeq
         | CmdWhile Exp Cmd
    deriving (Show)
    
evalCmd:: Cmd -> Mem -> Mem
evalCmd CmdSkip m = m
evalCmd (CmdSeq c c') m = ((evalCmd c') . (evalCmd c)) m -- ===evalCmd c' (evalCmd c m)
evalCmd (CmdAsg v e) m = update m v (evalExp e m)
evalCmd (CmdIf e c c') m = 
            (if evalExp e m /= 0 then evalCmd c else evalCmd c') m
evalCmd (CmdWhile e c) m = w m
            where w m = if evalExp e m == 0 then m else w (evalCmd c m) -- Não há garantia que não entrará em loop, cabe ao probramador cuidar disso
