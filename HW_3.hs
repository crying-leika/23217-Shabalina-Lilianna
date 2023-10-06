

-- 2 
func_perimeter :: [(Double, Double)] -> Double 
func_perimeter [(x1, y1), (x2, y2), (x3, y3)] = 
    sqrt (((x2 - x1)**2) + ((y2 - y1)**2)) + sqrt (((x3 - x1)**2) + ((y3 - y1)**2)) + sqrt (((x3 - x2)**2) + ((y3 - y2)**2))



-- 1
computeFibb :: Int -> Int
computeFibb 0 = 1
computeFibb 1 = 1
computeFibb n =
    computeFibb( n - 1 ) + computeFibb(n - 2)
fibMod5 = [computeFibb x | x <- [1..], computeFibb x `mod` 5 == 0] 
-- после 2ого запуска take 7 ghci сразу вывел список элементов удовлетворяющих условию, т. е. второй запуск работает быстрее

-- 3
checkAllEq :: Eq a => [a] -> Bool
checkAllEq [] = True
checkAllEq [x] = True
checkAllEq (x:y:xs) = x == y && y `elem` (xs) && checkAllEq (y:xs)

-- 5
compute_program_instraction :: String -> Double -> Double
compute_program_instraction s p | (s == "inc") = p + 1
                                | (s == "dec") = p - 1
                                | (s == "sqrt")  = sqrt (p)
                                | (s == "double") = p * 2
                                | (s == "halveIfPositive") = if (p > 0) then (p / 2) else p
                                | otherwise = error "error :("

computeProgram :: [String] -> Double -> Double
computeProgram [] p = p -- если [program] == 0, число не меняется 
computeProgram (program_frst : program) p = computeProgram program (compute_program_instraction program_frst p) 
--                                          вместо первого аргумента profram_frst после первой итерации - program появляется другая команда
--                                          которая работает с p', где p' - число р после выполения некоторых команд 


-- 4, можно ли считать это чистым списыванием, если основа кода не моя.... 
interval :: (Double, Double) -> (Double, Double) -> Double
interval (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

min_interval :: [(Double, Double)] -> Double
min_interval points = minimum [interval p1 p2 | p1 <- points, p2 <- points, p1 /= p2]

