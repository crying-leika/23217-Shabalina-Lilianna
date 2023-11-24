-- In file sem9_HW.hs --


--module Sem9_HW where
import Utils -- в импортах названия модулей не должны содержать .hs
             -- т.к. импорт делается по названию модуля, а не файлаф
-- когда импортируем весь файл не обязательно ведь указывать какие конкретно мы функции импортируем..? 

-- ошибка, вызванная импортированием 
-- sem9_HW.hs:4:8: error: parse error on input `Utils.hs' 
-- 4 | import Utils.hs
--   |        ^^^^^^^^


newtype Stack = Stack [Int]
    deriving Show

--1 задание 

-- Сконструировать пустой стек
emptyStack :: Stack
emptyStack = Stack []

-- Добавить элемент на вершину стека
-- Возвращается новое состояние стека
push :: Stack -> Int -> Stack
push (Stack list) a = Stack (a:list)

-- Взять элемент с вершины стека
-- Возвращается кортеж (верхний_элемент, новое_состояние_стека)
pop :: Stack -> (Int, Stack)
pop (Stack []) = error "empty Stack"
pop (Stack (head_list:list)) = (head_list, Stack list)


--2

data Instruction =
    Push Int
    | Add 
    | Sub 
    | Div 
    | Mul 
    | Pow 
    deriving Show

add' :: [Int] -> [Int]
add' (a:b:listt) = (a + b):listt

sub' :: [Int] -> [Int]
sub' (a:b:listt) = (b - a):listt

mul' :: [Int] -> [Int]
mul' (a:b:listt) = (a * b):listt

div' :: [Int] -> [Int]
div' (a:b:listt) = (b `div` a):listt

pow' :: [Int] -> [Int]
pow' (a:b:listt) = (b ^ a):listt

computeInstructions :: [Instruction] -> Int
computeInstructions instructions = computeInstructions' instructions (Stack [])

computeInstructions' :: [Instruction] -> Stack -> Int
computeInstructions' [] (Stack [result]) = result
computeInstructions' (Push x : list) (Stack stack) = computeInstructions' list (Stack (x : stack))
computeInstructions' (Add : list) (Stack (a : b : stack)) = computeInstructions' list (Stack ((a + b) : stack))
computeInstructions' (Sub : list) (Stack (a : b : stack)) = computeInstructions' list (Stack ((b - a) : stack))
computeInstructions' (Div : list) (Stack (a : b : stack)) = computeInstructions' list (Stack ((b `div` a) : stack))
computeInstructions' (Mul : list) (Stack (a : b : stack)) = computeInstructions' list (Stack ((a * b) : stack))
computeInstructions' (Pow : list) (Stack (a : b : stack)) = computeInstructions' list (Stack ((b ^ a) : stack))


-- 3

{-
--module Utils ( 
  --  split, strIsNumber, strToInt
--) where
-- import Data.Char
split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = split xs delim

strIsNumber :: String -> Bool
strIsNumber = all isDigit


strToInt :: String -> Int
strToInt s | strIsNumber s = read s
           | otherwise = error "This string is not a number" 

-}


parseString :: String -> [Instruction]
parseString str = let string = split str ' ' in (foldl (\acc a -> (instr a):acc) [] string)
    where
         instr a | strIsNumber a = Push (strToInt a) -- кладем число на стек
                 | a == "+" = Add -- все, что не число, то операция над числами
                 | a == "-" = Sub
                 | a == "/" = Div
                 | a == "*" = Mul
                 | a == "^" = Pow

--протестировать эту функцию не вышло, вечно ошибки компиляции
-- не понятно ghci, что такое isDigit из Utilis.hs 
-- потом не понятно стало, ругается он, что не знает Data.Char 
-- не знаю что не так, наверно сама где-то накосячила, а обвиняю ghci :) 

--4 

class Parsable a where
    parse :: a -> [Instruction]

instance Parsable String where
    parse :: String -> [Instruction]
    parse str = let string = split str ' ' in (foldl (\acc x -> (instr x):acc) [] string) 
        where instr a | strIsNumber a = Push (strToInt a) 
                      | a == "+" = Add
                      | a == "-" = Sub
                      | a == "/" = Div
                      | a == "*" = Mul
                      | a == "^" = Pow

instance Parsable [Instruction] where
    parse :: [Instruction] -> [Instruction]
    parse instrr = instrr 


stackMachine :: (Parsable a) => a -> Int
stackMachine instr = computeInstructions instructionss
    where 
        instructionss = (parse instr)








