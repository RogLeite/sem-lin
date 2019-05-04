data RE = REEmpty
        | REEpsilon
        | REChar Char
        | REOr RE RE
        | RESeq RE RE
        | REKleene RE
            
type Ling = String -> Bool

eval:: RE -> Ling
eval REEmpty _ = False
eval REEpsilon l = null l
eval (REChar c) l = (l == [c])
eval (REOr e e') l = (eval e l) || (eval e' l)
eval (RESeq e e') l = any f [0 .. length l]
    where f i = (eval e (take i l)) && (eval e' (drop i l))
eval es@(REKleene e) l = (null l) || any f [1 .. length l]
    where f i = (eval e (take i l)) && (eval es (drop i l))
