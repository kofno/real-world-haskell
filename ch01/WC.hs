-- file: ch01/WC.hs
-- limes beginning with "--" are comments

main = interact wordCount
-- Counts chars version
     where wordCount input = show (length input) ++ "\n"

-- Counts words version
--     where wordCount input = show (length (words input)) ++ "\n"

-- Counts lines version
--     where wordCount input = show (length (lines input)) ++ "\n"