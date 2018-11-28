fib_guard n | n < 2 = 1
            | otherwise = fib_guard(n - 1) + fib_guard(n - 2)
