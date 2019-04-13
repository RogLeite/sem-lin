fib = fib' 1 1
    where fib' a b = a:(fib' b (a + b))
