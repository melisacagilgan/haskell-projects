--Part 1
{-1. Implement a function which takes a list and returns the number of numeric characters in
the list. Try to solve this question in two different ways, one with recursion and one with list
comprehension.-}

numList = "0123456789"

countNumbersRecursive [] = 0
countNumbersRecursive (x:xs) = if x `elem` numList then 1 + countNumbersRecursive xs
                               else countNumbersRecursive xs

countList [x] = 1
countList (x:xs) = 1 + countList xs

countNumbersRecursive2 x = countList ([a | a <- x, a `elem` numList])


{-2. Implement a function which takes a j value and calculate the result of the following
equation;-}

sumEquation 0 = 6
sumEquation j = (j*j+6)/(2*j+1) + sumEquation (j-1)


--Part 2
{-1. The implementations of the letInFunction, lambdaQuestion and lambdaQuestion
functions can be found below. You need to trace the following functions and to provide
the output of them for the following Haskell function calls.-}

{-  (f 2 = 1 + 4) + (f 3 = 1 + 5) = 11  -}
letInFunction = let a = 1
                    f x = a + (g x)
                    g x = x + 2
                in f 2 + let a = 4
                             g x = (x + 1)
                         in (f 3)

{-  [2+3, 4+3, 6+3] = [5,7,9]    -}
mapQuestion xs = map f xs where f x = x * 2 + 3

{- 1+(2+(3+(1))) = 7    -}
lambdaQuestion xs = foldr (\x y -> x + y) 1 xs


{-2. Implement the set union, the set intersect and set difference functions using higher
order functions. You can also try to implement a function that takes union of two lists
without the intersection (rest).-}

removeSameElem [] y = []
removeSameElem (x:xs) y = if x `elem` y then removeSameElem xs y
                        else [x] ++ removeSameElem xs y
setUnion x y = (removeSameElem x y) ++ y

getSameElem [] y = []
getSameElem (x:xs) y = if x `elem` y then [x] ++ getSameElem xs y
                        else  getSameElem xs y
setIntersection x y = getSameElem x y

takeDifference [] y = []
takeDifference (x:xs) y = if x `notElem` y then [x] ++ takeDifference xs y
                          else takeDifference xs y
setDifference x y = takeDifference x y

setRest x y = takeDifference x y ++ removeSameElem y x