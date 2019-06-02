module Cont_Imperative_Lang where

type Var = String
type Value = Integer
type Mem = Var -> Value

isTrue:: Value -> Bool
isTrue v = v /= 0

initMem:: Mem
initMem = const 0

{---------------------------------------------------------------}
data Exp = ExpAdd Exp Exp
        | ExpMul Exp Exp
        | ExpNum Value
        | ExpVar Var
    deriving (Show)

evalExp:: Exp -> Mem -> Value
evalExp (ExpNum n) m = n
evalExp (ExpAdd e e') m = (evalExp e m) + (evalExp e' m)
evalExp (ExpMul e e') m = (evalExp e m) * (evalExp e' m)
evalExp (ExpVar e) m = m e
{-------------------------------------------------------------}
update:: Mem -> Var -> Value -> Mem
update m var val = \x -> if x == var then val else m x

{-------------------------------------------------------------}

data Cmd = CmdAsg Var Exp
         | CmdIf Exp Cmd Cmd
         | CmdSeq Cmd Cmd
         | CmdSkip          --Elemento Neutro de CmdSeq
         | CmdWhile Exp Cmd
    deriving (Show)

evalCmd:: Cmd -> Mem -> (Mem -> Value) -> Value
evalCmd CmdSkip m k = k m
evalCmd (CmdSeq c c') m k = evalCmd c m (\m' -> evalCmd c' m' k)
evalCmd (CmdAsg v e) m k = k (update m v (evalExp e m))
evalCmd (CmdIf e c c') m k =
            (if isTrue (evalExp e m) then evalCmd c else evalCmd c') m k
evalCmd (CmdWhile e c) m k = k' m
            where k' m = if isTrue(evalExp e m) then evalCmd c m k' else k m


--evalCmd (CmdAsg "x" (ExpNum 1)) initMem (\m -> m "x")
