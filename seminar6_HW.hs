--1
reverse' :: [a] -> [a]
reverse' [] = []
reverse' a = foldl (\s x -> (x:s)) [] a

--2

evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly a = foldl (\acc x -> if frs (acc) == 1 then frs(acc = 0) snd(acc) ++ x else frs (acc = 1)) (1, [])
 
-- я честно пыталась, но понять как нормально менять состояние первого элемента аккумулятора-кортежа не смогла
-- а поздно вечером писать не культурно :(, но и нагуглить я не смогла как это делать 

--3 

for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (i, a) inc_func bool_func for_a = if (bool_func) then for (((inc_func i), (for_a a)) (inc_func) (bool_func)) else a

-- 4

dec_mult :: [(a, b)] -> [(a, b)]
dec_mult pepe = [p1 p2 | p1 <- pepe, p2 <- pepe]

-- 5

type BinaryRelation a = Eq a => [(a, a)]

refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl [] n = True
refl (x:xs) n = if (n `elem` (x, x)) then refl xs n else False



sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim n m = sim_Helper m m

sim_help :: (Eq a, Eq b) => [(a, b)] -> [(b, a)] -> Bool
sim_help [] m = True
sim_help ((x, y):xs) m = if (m `elem` (y, x)) then simHelper xs m else False

