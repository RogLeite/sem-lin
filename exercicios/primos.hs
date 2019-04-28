inf_int:: [Integer]
inf_int = aux 1
    where 
        aux x = x:(aux (x+1))

at:: Integer -> [Integer] -> Integer
at _ [] = 0
at 1 (x:xs) = x
at a (x:xs) = at (a-1) xs

first::[Integer] -> Integer
first = at 1


--PRIMOS----------------------------------------------------
primos:: [Integer]
primos = aux 2 inf_int
    where
        aux a list = a:(aux (first filtered) filtered)
            where
                --filtered é a lista infinita com todos os multiplos de primos menores que a filtrados
                filtered = filter ( /* a ) (filter (> a) list)
                    where
                        --OPERATOR--NOT MULTIPLE-------------------------------------
                        (/*):: Integer -> Integer -> Bool
                        (/*) x a = (x `mod` a) /= 0

                        --OPERATOR--/* OR LARGER---------------------------------------
                        (/*|>):: Integer -> Integer -> Bool
                        (/*|>) x a = (x > a) || (x /* a)
                
