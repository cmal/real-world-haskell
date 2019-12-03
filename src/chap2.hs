lastButOne :: [a] -> a
lastButOne xs = head $ drop (length xs - 2) xs
-- > lastButOne [1]
-- 1
-- > lastButOne []
-- *** Exception: Prelude.head: empty list
