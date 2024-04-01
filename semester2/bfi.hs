import System.IO (hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Control.Monad.State
import Data.Char (chr, ord)

-- список из нулей представляющий ленту памяти, хаскелл ленивый, определит когда надо остановиться сам 
data' :: [Int]
data' = [y | x <- [1..], y <- [0*x]]

out' :: String
out' = []

type BrainFState = (String, String, [Int], String, Int, Int, Int, [Int])

main :: IO ()
main = do
    temp <- getArgs
    program <- readFile $ head temp

    let in' = foldl (\acc x -> acc ++ x ++ " ") [] (tail temp) :: String

    putStr (evalState brainFck (program, in', data', out', 0, 0, 0, []))

brainFck :: State BrainFState String
brainFck = do
    (program, in', data', out', dataPtr, programPtr, depth, stack) <- get
    let newState = brainHelper (program, in', data', out', dataPtr, programPtr, depth, stack)
    put newState

    put ( brainHelper newState) -- выполнение одного шага интерпретатора

    if programPtr == (length program)
        then return out'
        else brainFck

brainHelper :: BrainFState -> BrainFState
brainHelper (program, in', data', out', dataPtr, programPtr, depth, stack) =
    case command of -- Выбор операции в зависимости от текущей команды программы
        '>' -> (program, in', data', out', dataPtr+1, programPtr+1, depth, stack)
        '<' -> (program, in', data', out', dataPtr-1, programPtr+1, depth, stack)
        '+' -> (program, in', updData data' dataPtr ((data' !! dataPtr) + 1), out', dataPtr, programPtr+1, depth, stack)
        '-' -> (program, in', updData data' dataPtr ((data' !! dataPtr) - 1), out', dataPtr, programPtr+1, depth, stack)
        '.' -> (program, in', data', out' ++ [chr (data' !! dataPtr)], dataPtr, programPtr+1, depth, stack)
        ',' -> (program, tail in', updData data' dataPtr (ord $ head in'), out', dataPtr, programPtr+1, depth, stack)
        '[' -> if data' !! dataPtr == 0
                then (program, in', data', out', dataPtr, getCloseBracketIndex program programPtr, depth, stack)
                else (program, in', data', out', dataPtr, programPtr+1, depth+1, programPtr : stack)
        ']' -> if data' !! dataPtr /= 0
                then (program, in', data', out', dataPtr, head stack, depth, tail stack)
                else (program, in', data', out', dataPtr, programPtr+1, depth, stack)
    where command = program !! programPtr

updData :: [Int] -> Int -> Int -> [Int]  -- функция обновления значения в ячейке данных
updData data' index upd = part1 ++ (upd:tail part2)
    where (part1, part2) = splitAt index data'

getCloseBracketIndex :: [Char] -> Int -> Int
getCloseBracketIndex program start = head (foldl func [(-1), 0, 0, 0] program)
    where func [ans, depth, i, flag] x
            | i < start = [ans, depth,   i+1, flag]
            | flag == 1 = [ans, depth,   i+1, flag]
            | x == '['  = [ans, depth+1, i+1, flag]
            | x == ']'  = [ans, depth-1, i+1, flag]
            | depth == 0= [i,   depth,   i+1, 1   ]
            | otherwise = [ans, depth,   i+1, flag]