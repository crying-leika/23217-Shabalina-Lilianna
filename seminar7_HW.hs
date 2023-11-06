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
-- выше комментарий за кукую-то из домашек, вроде к 5му семинару
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


--это все, что мой мозг смог выдать к вечеру понедельника
--я постараюсь к пятнице еще порешать задачи, очень мне не хочется за семестр получать 3
--но мегатяжело идет решение задач :( 