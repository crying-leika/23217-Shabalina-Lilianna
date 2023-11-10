-- In file sem9_HW.hs --
--module Sem9_HW where

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

add' :: Stack -> Stack 
add' (Stack (a:b:list)) = Stack ((a + b):list)

Sub :: Stack -> Stack
Sub (Stack (a:b:list)) = Stack ((a - b):list)

div :: Stack -> Stack 
div (Stack (a:b:list)) = Stack ((b/a):list)

mul :: Stack -> Stack 
mul (Stack (a:b:list)) = Stack ((b*a):list)

pow :: Stack -> Stack 
pow (Stack (a:b:list)) = Stack ((b^a):list)






--computeInstructions :: [Instruction] -> Int
--computeInstructions arr_of_instraction = ans where 
    --ans = Stack (ans:_) = foldl (\acc x -> (func acc x)) emptyStack arr_of_instraction
--computeInstructions [comand] = foldl (\acc + x -> )

-- 3
--parseString :: String -> [Instruction]
