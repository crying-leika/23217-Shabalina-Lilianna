-- 1
data PeanoNatural = Zero | Succ PeanoNatural

dup :: [a] -> [a]
dup [] = []
dup (x:xs) = x : x : dup xs

dup' :: [a] -> [a]
dup' list = foldr (\x acc -> x:x:acc) [] list 

--2 

--data PeanoNatural = Zero | Succ PeanoNatural

natToInt :: PeanoNatural -> Int
natToInt Zero = 0
natToInt (Succ count) = 1 + natToInt count

--ghci> :load sem0.hs
--[1 of 2] Compiling Main             ( sem0.hs, interpreted )
--Ok, one module loaded.
--ghci> natToInt Zero
-- <interactive>:19:1: error:
--    Variable not in scope: natToInt :: t0 -> t
-- <interactive>:19:10: error: Data constructor not in scope: Zero

-- =( 
