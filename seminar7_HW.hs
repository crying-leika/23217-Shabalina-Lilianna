import Data.Array
newtype Matrix = Matrix (Array (Int, Int) Double)
--    deriving Show

--1 конструирование матрицы

example1 :: [((Int, Int), Double)]
example1 = [((0,0), 1), ((0,1), -2), ((0,2), 3),
            ((1,0), 4), ((1,1), 0), ((1,2), 6),
            ((2,0), -7), ((2,1), 8), ((2,2), 9)]
example2 :: [((Int, Int), Double)]
example2 = [((0,0), 13), ((0,1), 0),
            ((1,0), 0 ), ((1,1), 12)]
m1 = makeMatrix (3, 3) example1 -- m1 :: Matrix
m2 = makeMatrix (2, 2) example2 -- m2 :: Matrix

makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix (a, b) example1 = Matrix(array ((0, 0),(a-1, b-1)) example1)

--1 обращение к элементу по индексу

(!!!) :: Matrix -> (Int, Int) -> Double
--(!!!) example1 (a, b) = array(example1) ! (a, b)
(!!!) (Matrix arr) (a, b) = (arr) ! (a, b)

-- 1 получение размера матрицы 

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix mat) = (c-a+1, d-b+1) 
    where ((a,b), (c,d)) = bounds mat

-- 1 список всех индексов элементов матрицы

matrixIndices :: Matrix -> [(Int, Int)]
matrixIndices (Matrix mat) = indices mat 

-- 2 свертка по элементам
--                i    j   element
type MtxElem = ((Int, Int), Double)
matrixFold :: (b -> MtxElem -> b) -> b -> Matrix -> b
--matrixFold function acc (()) = acc -- если убрать эту строчку, то задача скомпилиться, но я не знаю чем эта тсрока не угодила, если вместо (()) ствить [] или () так же не компилится
matrixFold function acc mat = foldl function acc (make_list mat)
    where 
        make_list :: Matrix -> [MtxElem]
        make_list (Matrix mat) = fst(foldl (\(acc, index:indexes) x -> ((acc ++ [(index, x)]), indexes)) ([], indices mat) (elems mat))

--foldl :: (β → α → β) → β → ([α] → β)     это просто моя подсказка из интернета, как можно обычную свертку написать. 
--foldl f v [ ] = v
--foldl f v (x : xs) = foldl f (f v x) xs


-- map 
-- computeVector array = foldl (\s x -> s ++ [computeVec_helper array x]) [] -- похоже на переизобретение map
-- выше комментарий за какую-то из домашек, вроде к 5му семинару
-- как раз задача map переизобрести 

matrixMap :: (MtxElem -> Double) -> Matrix -> Matrix
matrixMap f (Matrix arr) = Matrix (array (bounds arr) [((i, j), f ((i, j), arr ! (i, j))) | (i, j) <- indices arr]) -- одна строчка заняла почти 2 часа, как люди вообще функциональщину понимают...

--3 
-- Сигнатура функции unwords выглядит следующим образом:
--unwords :: [String] -> String 
--Функция unwords объединяет список строк, разделяя их пробелами. Она удаляет все пробелы в начале и конце каждой строки, а также вставляет пробел между каждой парой строк.
--Например, unwords ["Hello", "world"] вернет "Hello world", а unwords ["I", "am", "a", "programmer"] вернет "I am a programmer".
--unlines :: [String] -> String
--создает строку из массива строк, вставляет новые символы строки между исходными строками



-- вышенаписанное нечто это то, что мне помог найти гугль
-- вставила сюда, чтобы вопросов не было откуда взялось такое непотянное решение, ведь об этих функциях вроде не говорили на лекциях

instance Show Matrix where
    show (Matrix arr) = unlines [unwords [show (arr ! (i, j)) | j <- [minJ..maxJ]] | i <- [minI..maxI]]
        where
            ((minI, minJ), (maxI, maxJ)) = bounds arr

-- 4 
transpose :: Matrix -> Matrix
transpose (Matrix arr) =
    let ((minI, minJ), (maxI, maxJ)) = bounds arr
        transposed = array ((minJ, minI), (maxJ, maxI)) [((j, i), arr ! (i, j)) | i <- [minI .. maxI], j <- [minJ .. maxJ]]
    in Matrix transposed
--transposed - новый двумерный массив  

-- спасибо тому дню, когда я выдумала задачу из головы и подходила к вам, чтобы понять как работает let in
--foo :: [Int] -> Int
--foo l = let s = map (\x -> x*x) l in
  --  foldl(\acc x -> x + acc ) 0 s 

--5 

instance Eq Matrix where
    --т к ласс типов Eq определяет операции сравнения на равенство для типов данных.
    --создаем экземпляр реализации функции == для матриц 
    (==) :: Matrix -> Matrix -> Bool
    mat1 == mat2   | matrixSize mat1 == matrixSize mat2 = matrixFold (\acc ((i, j), x) -> if acc == True then (x == (mat2 !!! (i, j))) else False) True mat1
                   | otherwise = False

-- 6 

instance Num Matrix where
   (+) :: Matrix -> Matrix -> Matrix
   mat1 + mat2 = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), x + (mat2 !!! (i, j)))]) [] mat1)

-- задание реализовать умножение матриц умножило меня на 0, не получилось

negate::Matrix -> Matrix 
negate (Matrix mat) = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), x * (-1))]) [] (Matrix mat))

--negate (Matrix mat) = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), x * (-1))]) [] mat)
-- эта вот строка не давала компилятору покоя, выдавал ошибку 
--seminar7_HW.hs:109:87: error:
--    * Couldn't match expected type `Matrix'
--                  with actual type `Array (Int, Int) Double'
--    * In the third argument of `matrixFold', namely `mat'
--      In the expression:
--        matrixFold
--          (\ acc ((i, j), x) -> acc ++ [((i, j), x * (- 1))]) [] mat
--      In an equation for `Main.negate':
--          Main.negate (Matrix mat)
--            = matrixFold
--                (\ acc ((i, j), x) -> acc ++ [((i, j), x * (- 1))]) [] mat
--    |
--109 | negate (Matrix mat) = matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), x * (-1))]) [] mat

-- не очень понимаю чем сильно помогло изменить mat на (Matrix mat), притом что перед сверткой стояла функция makeMatrix
-- т е после всех манипуляций в любом случае должна вернуться матрица 

abs:: Matrix -> Matrix 
abs (Matrix mat) = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> if (x >= 0) then  acc ++ [((i, j), x * (1))] else acc ++ [((i, j), x * (-1))]) [] (Matrix mat))

signum :: Matrix -> Matrix 
signum (Matrix mat) = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> if (x >= 0) then  acc ++ [((i, j), a)] else acc ++ [((i, j), b)]) [] (Matrix mat))
    where 
        a = 1 
        b = (-1)

--fromInteger :: Integer -> Matrix 
--fromInteger count = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), a)]) [] (Matrix example1))
--    where 
--        a = count


-- не получается избежать ошибки, без слова (Matrix), заменой Matrix example1 на mat 
-- не руботает 

--Couldn't match expected type: Array (Int, Int) Double
--                  with actual type: [((Int, Int), Double)]
--    * In the first argument of `Matrix', namely `example1'
--      In the third argument of `matrixFold', namely `(Matrix example1)'
--      In the second argument of `makeMatrix', namely
--        `(matrixFold
--            (\ acc ((i, j), x) -> acc ++ [((i, j), a)]) [] (Matrix example1))'
--    |
--140 | fromInteger count = makeMatrix (0, 0) (matrixFold (\acc ((i, j), x) -> acc ++ [((i, j), a)]) [] 
--(Matrix example1))


-- 7 

det2 :: Matrix -> Double
det2 (Matrix mat) = (mat ! (0, 0)) *(mat ! (1, 1)) - (mat ! (0, 1))*(mat ! (1, 0))
--    ((0, 0), _ )*((1, 1) _ ) - ((0, 1) _ )*((1, 0) _)


det3 :: Matrix -> Double
det3 (Matrix mat) = (mat ! (0, 0))*(mat ! (1, 1))*(mat ! (2, 2)) + (mat ! (0, 1))*(mat ! (1, 2))*(mat ! (2, 0)) - (mat ! (0, 2))*(mat ! (2, 2))*(mat ! (2, 0)) + (mat ! (0, 2))*(mat ! (2, 0))*(mat ! (2, 1)) - (mat ! (0, 1))*(mat ! (1, 0))*(mat ! (2, 2)) - (mat ! (0, 0))*(mat ! (1, 2))*(mat ! (2, 1))


-- 8 

isDiagonal :: Matrix -> Bool
isDiagonal (Matrix mat) = matrixFold (\acc ((i, j), el) -> if i==j then acc else if (mat ! (i, j)) == 0 then acc else False) True (Matrix mat)

isSymmetrical :: Matrix -> Bool
isSymmetrical (Matrix mat) = matrixFold (\acc ((i, j), el) -> if ((mat ! (i, j)) == (mat ! (j, i))) then acc else False) True (Matrix mat)


