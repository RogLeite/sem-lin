import Functional_Lang


main = do putStrLn "Insert a number. will double"
          num <- getLine
          let number = read num :: Integer
          putStrLn (
              show (
                evalExp (ExpApp (ExpLambda "x" (ExpMul (ExpK 2) (ExpVar "x"))) (ExpK number)) initEnv
              )
            )



-- main = do putStrLn "Insert a number. will Build show with app"
--           num <- getLine
--           let number = read num :: Integer
--           putStrLn (
--               show (
--                 evalExp (ExpApp (ExpLambda "x" (ExpVar "x")) (ExpK number)) initEnv
--               )
--             )

-- main = do putStrLn "Insert a number. will Echo"
--           num <- getLine
--           let number = read num :: Integer
--           putStrLn (
--               show (
--                 evalExp (ExpK number) initEnv
--               )
--             )


-- main = do putStrLn "Insert a number. will show 0"
--           num <- getLine
--           let number = read num :: Integer
--           putStrLn (
--               show (
--                 evalExp (ExpK 0) initEnv
--               )
--             )



--putStrLn (show (evalExp ((ExpApp (ExpLambda "x" (ExpK 3)) (ExpK number)) initEnv)) )
