-- file: ch02/lastButOne.hs
-- a bit of a cheat, since I've done LYAHFGG before

lastButOne :: [a] -> a
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
lastButOne _ = error "Need at least two items in the list"