{-a. Using recursion, implement a function that shows first x natural numbers.-}

natural 1 = [1]
natural x = natural (x-1) ++ [x]

{-b. Implement the power function recursively without using ** or ^ operator, where
power x y evaluates xy.-}

powf x 0 = 1
powf x y = x * powf x (y-1)

{-c. Implement f(x) by using pattern matching (Explained in 2). (Without using if..else
statements-}

fx 5 = 1-5
fx 10 = 1/10
fx x = x

{-d. Write a function that takes a string and returns the number of vowels in the string.-}

vowels = "aeiouAEIOU"
findVowels [] = 0
findVowels (x:xs) = if x `elem` vowels then 1 + findVowels xs else findVowels xs

{-e. Write a function that takes two lists where one list contains letter grades, and
another list contains the credits. You function should calculate and then return
the GPA (Grade Point Average). The weight of the letter grades are as follow:
A = 4, B = 3, C = 2, D= 1 and F = 0.-}

calcGPA _ [] = 0
calcGPA [] _ = 0
calcGPA (x:xs) (y:ys) = case x of 'A' -> (4*y + calcGPA xs ys)
                                  'B' -> (3*y + calcGPA xs ys)
                                  'C' -> (2*y + calcGPA xs ys)
                                  'D' -> (1*y + calcGPA xs ys)
                                  'F' -> (0*y + calcGPA xs ys)
                                  otherwise -> 0

creditSum [x] = x
creditSum (x:xs) = x + creditSum xs

calculateGPA x y = (calcGPA x y) / (creditSum y)