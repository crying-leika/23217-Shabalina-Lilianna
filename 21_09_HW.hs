-- четность 
even' :: Int -> Bool
even' count' =  
    if count' `mod` 2 == 0 then  True else False
 

odd' :: Int -> Bool
odd' count'' =  
    if count'' `mod` 2 == 0 then  False else True

-- Посчитать N-е число Фибоначчи 
computeFibb :: Int -> Int
computeFibb 0 = 0
computeFibb 1 = 1
computeFibb n = computeFibb( n - 1 ) + computeFibb(n - 2)
-- Посчитать сумму нечетых чисел Фибоначчи от 1 до N 
-- sumOddFibb :: Int -> Int


sumOddFibonacci :: Int -> Int
sumOddFibonacci 0 = 0
sumOddFibonacci 1 = 1
sumOddFibonacci n =
    if n `mod` 3 == 0 then n - 1 else n 
sumOddFibonacci summ = computeFibb n + computeFibb (n - 1)
