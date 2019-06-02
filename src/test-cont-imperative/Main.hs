import Cont_Imperative_Lang
showX:: Mem -> Value
showX m = m "x"

showNum = do  putStrLn "Insert a number. will show"
              num <- getLine
              let number = read num :: Integer
              putStrLn (show (evalCmd (CmdAsg "x" (ExpNum number)) initMem showX ) )

square = do putStrLn "Insert a number, will square"
              num <- getLine
              let number = read num :: Integer
              putStrLn (show (evalCmd (CmdSeq (CmdAsg "y" (ExpNum number)) (CmdAsg "x" (ExpMul (ExpVar "y") (ExpVar "y")) )) initMem showX ) )




main = square
