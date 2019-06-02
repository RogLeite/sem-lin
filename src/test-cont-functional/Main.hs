import Cont_Imperative_Lang
showX:: Mem -> Value
showX m = m "x"

main = do  putStrLn "Insert a number. will show"
              num <- getLine
              let number = read num :: Integer
              putStrLn (show (evalCmd (CmdAsg "x" (ExpNum number)) initMem showX ) )
