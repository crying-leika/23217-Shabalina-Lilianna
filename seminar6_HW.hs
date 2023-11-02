--1
reverse' :: [a] -> [a]
reverse' [] = []
reverse' a = foldl (\s x -> (x:s)) [] a

--2

evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly a = foldl (\(idx, acc) x -> if idx == 1 then (0, acc ++ x) else (1, acc)) (1, [])

--3 

for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (i, a) inc_func bool_func for_a = if (bool_func i) then for ((inc_func i), (for_a i a)) (inc_func) (bool_func) for_a else

-- 4

mult::(a, b) -> (a, b) -> (a, b) -> (a, b)
mult (x1, y1) (x2, y2) = (x1, y2) (x2, y1)

dec_mult :: [(a, b)] -> [(a, b)]
dec_mult pepe = [ mult p1 p2 | p1 <- pepe, p2 <- pepe]

-- 5

type BinaryRelation a = Eq a => [(a, a)]

refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl [] n = True
refl (x:xs) n = if (n `elem` (x, x)) then refl xs n else False



sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim n m = sim_Helper m m

sim_help :: (Eq a, Eq b) => [(a, b)] -> [(b, a)] -> Bool
sim_help [] m = True
sim_help ((x, y):xs) m = if (m `elem` (y, x)) then sim_help xs m else False

