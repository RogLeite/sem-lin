import Lang
mainMem:: Mem
mainMem = initMem

main = do putStrLn "Insert a number. If /= 0 then 1 else 2"
          num <- getLine
          let number = read num :: Integer
          putStrLn (show ( evalCmd (CmdSeq (CmdAsg "a" (ExpNum number)) (CmdIf (ExpVar "a") (CmdAsg "b" (ExpNum 1)) (CmdAsg "b" (ExpNum 2))) ) mainMem  "b" ) )
