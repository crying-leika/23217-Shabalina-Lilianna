-- 1 реализальция map 
map'::  (a -> b) -> [a] -> [b]
map' f a = foldl (\s x -> s++[f x]) [] a

map''::(a -> b) -> [a] -> [b]
map'' f a = foldr (\x s -> [f x] ++ s) [] a
-- 2
nub' :: Eq a => [a]->[a]
nub' [] = []
nub' (x:xs) = x : nub' (filter(\s -> s/=x) xs)
--nub'  filter (x `notElem` xs) && nub'(y:xs) что тут не так, почему оно не компилируется....

union' :: Eq a => [a] -> [a] -> [a]
union' [] [] = []
union' [] [a] = [a]
union' [a] [] = [a]
union' a0 a1 = nub' (a0++a1)

intersection':: Eq a => [a]->[a]->[a]
--intersection' [] [] = [] почему ghci на это ругается? 
--intersection' [] [a] =[] и на это..
--intersection' [a] [] = [] даже здесь что-то не так...
intersection' a1 a2 = filter (`elem` a1) a2
--ghci> filter (`elem` ['а'..'я']) "тЫ СМЕЕШЬСя, ВЕДЬ я ДрУГой"    книга Липовача - это прекрасно, без книги не поняла бы как делать. 
--"тяярой"
--ghci> filter (`elem` ['А'..'Я']) "я Смеюсь, Ведь ты такОЙ же"
--"СВОЙ"


-- 3 
computeVec_helper :: [String] -> Double -> Double
computeVec_helper [] a = a
computeVec_helper (s:s1) a
    |(s == "inc" ) = computeVec_helper s1 (a + 1.0)
    |(s == "dec" ) = computeVec_helper s1 (a - 1.0)
    |(s == "double") = computeVec_helper s1 (a * 2.0)
    |(s == "sqrt") = computeVec_helper s1 (sqrt a)
    |(s == "halveIfPositive") = if (a > 0) then computeVec_helper s1 (a / 2) else computeVec_helper s1 a

computeVector :: [String] -> [Double] -> [Double]
computeVector array = foldl (\s x -> s ++ [computeVec_helper array x]) [] 
-- s - является стартовым накопителем - пустным списком, куда позже складывается весь полученный массив и это является моей переменной array
 

-- 4

is_instruction:: String -> Bool
is_instruction _ = False
is_instruction "inc" = True
is_instruction "dec" = True
is_instruction "double" = True
is_instruction "sqrt" = True
is_instruction "halveIfPositive" = True

cleaner :: [String] -> [String]
cleaner blocks = filter is_instruction blocks


--5

-- что опять не так с этим вариантом... 
--optimizer :: [String] -> [String]
--optimizer (x:y:xs) = if (x == "inc" && y== "dec") || (x=="dec" && y == "inc") then optimizer (tail xs) else y : optimizer xs 
optimizer_helper :: [String] -> [String]
optimizer_helper "dec" ("inc":xs) = xs
optimizer_helper "inc" ("dec":xs) = xs
optimizer_helper x xs = x : xs

optimizer :: [String] -> [String]
optimizer = foldr optimize []

--6 

 -- f :: Double -> Double  получается это как "map :: (a -> b) -> [a] -> [b]"
 points :: [(Double,Double)] -> (Double -> Double) -> [(Double,Double)]
 --points [] f = [] на книгах по хаскеллю можно писать "ghci убивает", что опять не так... 
 points point f = foldl (\opt (x, y) -> if (f(x) < y) then opt ++ [(x, y)] else opt) [] point


-- я обязательно выживу... 
