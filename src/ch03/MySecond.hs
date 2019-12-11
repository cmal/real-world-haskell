-- file: ch03/MySecond.hs
mySecond :: [a] -> Maybe a

mySecond [] = Nothing
mySecond xs = if null (tail xs)
              then Nothing
              else Just (head (tail xs))


                   
tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
