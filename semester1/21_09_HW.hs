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
computeFibb n =
    if n <= 0 then error "ancorrect" 
    else computeFibb( n - 1 ) + computeFibb(n - 2)



-- Посчитать сумму нечетых чисел Фибоначчи от 1 до N 
sumOddFibbonacci :: Int -> Int
sumOddFibbonacci 1 = 1
sumOddFibbonacci n| n<=0 = error "ancorrect"
                  | odd(computeFibb n) = (computeFibb n) + sumOddFibbonacci(n - 1)
                  | otherwise = sumOddFibbonacci (n - 1)



-- магическое число
sumMagicNumber :: Int -> Int

sumMagicNumber n | n<=0 = error "ancorrect"
                 | n < 10 = n
                 | n > 10 = n `mod` 10 + sumMagicNumber(n `div` 10)
