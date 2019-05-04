data Exp = ExpAdd Exp Exp
        | ExpMul Exp Exp
        | ExpNum Integer
        
evalExp:: Exp -> Integer
evalExp (ExpNum n) = n
evalExp (ExpAdd e e') = (evalExp e) + (evalExp e')
evalExp (ExpMul e e') = (evalExp e) * (evalExp e')

