
-- 1 задача

func_HW :: (Double -> Double) -> (Double -> Double) -> Double -> Double
func_HW f g p | f(p) > g (p) = f(p)
              | f(p) == g(p) = f(p)
              | otherwise = g(p)

-- 2 задача

func_HW_2 :: (Double -> Double) -> Double -> Double
func_HW_2 = func_HW exp 

-- 3 задача 

func_HW_3 :: (Double -> Double) -> Double -> Double -> Double 
func_HW_3 f p n | n==0 = p
                | n<0 = error "error"
                | otherwise = func_HW_3 f (f p) (n-1)
                

                






