-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = interact wordCount
    -- where wordCount input = show (length (lines input)) ++ "\n"
    -- where wordCount input = show (sum (map (length.words) (lines input))) ++ "\n"
    where wordCount input = show (length input) ++ "\n"