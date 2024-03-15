import Control.Monad.State
-- 1 

fact' :: State (Int, Int) Int
fact' = do
  (step, acc) <- get
  if step == 0
    then return acc
    else do
      put (step - 1, acc * step) -- обновляем значения step и acc
      fact'

fact :: Int -> Int
fact n = evalState fact' (n, 1)

-- 2 

fibb' :: State (Int, Int, Int) Int
fibb' = do
  (step, n1, n2) <- get
  if step == 0
    then return n2
    else do
      put (step - 1, n1 + n2, n1)
      fibb'

fibb :: Int -> Int
fibb n = evalState fibb' (n, 1, 0)

--3

-- все мои попытки, увы, некомпилируются. Если на 3.5-4 балла 1 и 2 задача потянут, то будет очень славно
-- явно списывать не хочется, но не списывать - не можется :< 
