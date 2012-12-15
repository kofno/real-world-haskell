-- file: ch03/exercises.hs

-- #1: List exercises
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

-- #2: Tree exercises
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

tree1 = Node "root" (Nothing) (Nothing)
tree2 = Node "root" (Just (Node "left1" Nothing Nothing))
                    (Just (Node "right1" Nothing
                                         (Just (Node "right2" Nothing Nothing))))

-- myLength exercise
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- myMean exercise
myMean :: [Int] -> Float
myMean [] = 0.0
myMean xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

-- palindrome exercises
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- sort by length
lengthSort :: [[a]] -> [[a]]
lengthSort [] = []
lengthSort (x:xs) = (lengthSort lesser) ++ [x] ++ (lengthSort greater)
    where lengthx = length x
          lesser  = [ p | p <- xs, (length p) < lengthx ]
          greater = [ p | p <- xs, (length p) >= lengthx ]

-- intersperse
intersperse :: Char -> [[Char]] -> [Char]
intersperse _ [] = ""
intersperse c (x:[]) = x
intersperse c (x:xs) = x ++ [c] ++ (intersperse c xs)

-- tree depth
data BTree a = BNode a (BTree a) (BTree a)
             | Empty
               deriving (Show)

btreeDepth :: (Num a, Ord a) => BTree t -> a
btreeDepth Empty = 0
btreeDepth (BNode _ left right) =
    1 + max (btreeDepth left) (btreeDepth right)

-- expect 0
emptyBTree = Empty

-- expect 2
shortTree = BNode "root" Empty (BNode "right" Empty Empty)

-- expect 4
complexTree = BNode "root" (BNode "left1" (BNode "left1.l" Empty Empty)
                                          (BNode "left1.r" Empty
                                                           (BNode "deepest" Empty Empty)))
                           (BNode "right1" Empty (BNode "right1.r" Empty Empty))

-- 2D points exercises
data Point2D = Point2D Int Int
               deriving (Show)

data Direction = DStrait
               | DLeft
               | DRight
                 deriving (Show)

turn :: Point2D -> Point2D -> Point2D -> Direction
turn (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
     | crossProduct == 0 = DStrait
     | crossProduct >  0 = DLeft
     | crossProduct <  0 = DRight
     where crossProduct = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 -x1))

turns :: [Point2D] -> [Direction]
turns all@(x:y:z:_) = turn x y z : turns (tail all)
turns _ = []