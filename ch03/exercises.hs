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
    where lesser  = [ p | p <- xs, (length p) < (length x)]
          greater = [ p | p <- xs, (length p) >= (length x)]

-- intersperse
intersperse :: Char -> [[Char]] -> [Char]
intersperse _ [] = ""
intersperse c (x:[]) = x
intersperse c (x:xs) = x ++ [c] ++ (intersperse c xs)

-- tree depth [TO BE CONTINUED...]