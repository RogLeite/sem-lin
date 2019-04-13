inf_int:: [Integer]
inf_int = aux 1
    where 
        aux x = x:(aux (x+1))

fib:: [Integer]
fib = fib' 1 1
    where fib' a b = a:(fib' b (a + b))
    
at:: Integer -> [Integer] -> Integer
at _ [] = 0
at 1 (x:xs) = x
at a (x:xs) = at (a-1) xs
