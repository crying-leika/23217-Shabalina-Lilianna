data BinTree a =
    Nil |
    Node {
        left :: BinTree a,
        right :: BinTree a,
        value :: a,
        count :: Int
    }
-- для более удобного вывода
instance Show a => Show (BinTree a) where
    show = show0 0 where
    show0 _ Nil = "Nil"
    show0 lvl Node{left=l, right=r, value=v, count=cnt} =
        "Node (v = " ++ show v ++ ")\n" ++
        replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
        replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"


testTree :: BinTree Int
testTree = Node {
    left = Node {
        left = Node {left = Nil, right = Nil, value = 1, count = 1},
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 4, count = 1},
            right = Node {
                left = Nil, right = Nil, value = 7, count = 1},
            value = 6, count = 1
        },
        value = 3, count = 1
    },
    right = Node {
        left = Nil,
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 13, count = 1},
            right = Nil,
            value = 14, count = 1
        },
        value = 10, count = 1
    },
    value = 8, count = 1
}


-- 1

insert :: Ord a => BinTree a -> a -> BinTree a
insert Nil x = Node Nil Nil x 1
insert (Node left right value count) x
    | x < value = Node (insert left x) right value count
    | x > value = Node left (insert right x) value count
    | otherwise = Node left right value (count + 1)


fromList :: Ord a => [a] -> BinTree a
fromList arr = foldl (\acc x -> insert acc x) (insert Nil (head arr)) (tail arr) 

-- 2

findMin :: Ord a => BinTree a -> Maybe a
findMin Nil = Nothing
findMin (Node {left=Nil, value=v}) = Just v
findMin (Node {left=l, value=_}) = findMin l

findMax :: Ord a => BinTree a -> Maybe a 
findMax Nil = Nothing
findMax (Node {right=Nil, value=v}) = Just v
findMax (Node {right=r, value=_}) = findMax r

-- 3

rep :: Int -> a -> [a]
rep n x
    | n <= 0    = []
    | otherwise = x : rep (n - 1) x

treeSort :: Ord a => BinTree a -> [a]
treeSort Nil = []
treeSort (Node {left=l, right=r, value=v, count=c}) =
    treeSort l ++ rep c v ++ treeSort r

{-
как говорится:
списывай, но не точь-в-точь (снизу решение Богдана, на которое я опиралась, сама недодумалась до нормального решения)

treeSort :: Ord a => BinTree a -> [a]
treeSort Nil = []
treeSort tree = (treeSort l) ++ (repeat' tree) ++ (treeSort r) 
                where Node{left=l, right=r, value=v, count=cnt} = tree

repeat' ::  Ord a => BinTree a -> [a]
repeat' tree  = let Node{left=l, right=r, value=v, count=cnt} = tree in 
                fst(foldl (\(acc, val) x -> (val:acc, val)) ([], v) [1..cnt]) 
-} 


-- 4

{-
x <|> y :: m a
is a computation which "tries" x and if it fails then "tries" y. Such kinds of computation instantiate Alternative.


ссылка на объяснение, что за штуковина такая <|>:
https://stackoverflow.com/questions/23716613/what-is-in-haskell 
-}

findAny :: Ord a => (a -> Bool) -> BinTree a -> Maybe a
findAny _ Nil = Nothing
findAny pred (Node {left=l, right=r, value=v, count=с})
    | pred v       = Just v
    | otherwise = findAny pred l <|> findAny pred r --рекурсивная проверка сначала левого поддерева, потом правого
