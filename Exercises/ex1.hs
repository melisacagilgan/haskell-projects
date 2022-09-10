{-
a. Write a Haskell function that gets a list which consist of numbers and returns the
sum of the numbers in the list without the first and the last items.
-}

sumList [x] = 0
sumList (x:xs) = head xs + sumList (tail xs)

{-
b. Write a Haskell function called “body” that gets a list which consist of numbers
and returns the list without the first and last elements.
-}
bodyList [x] = [x]
bodyList (x:xs) = [(head xs)] ++ init xs

{-
c. Write a Haskell function that takes two lists and returns the maximum value in
these lists. For example, using following inputs: [3,2,5] [4,1,7] it should return 7
-}

maxVal xs ys =  if maximum xs >= maximum ys then maximum xs else maximum ys

{-
d. Modify the function above (c) to return a tuple that contains maximum values in
these lists. For example, using following inputs: [3,2,5] [4,1,7] it should return (5,7)
-}

maxVal2 xs ys =  [maximum xs] ++[maximum ys]

{-
e. Write a Haskell function that takes an item and a list. It then checks if the item
exists in the list. If the item exists in the list, the function returns the list directly. If the
item does not exist, the function adds the item to the list and returns the updated
list.
-}

checkList x ys = if x `elem` ys then ys else [x]++ys

{-
f. Write a Haskell function that takes a tuple with three items and returns the third
item.
-}

myTuple = ("CNG", 123, 'm')
thirdItem (_,_,x) = x

{-
g. Write a function takes two points as a tuple such as; (x1, y1) and (x2, y2) with their x
and y coordinates and calculate the distance between the points using the
following formula. Hint: you can use sqrt function.
-}

dist (x1,y1) (x2,y2) = sqrt((x1-x2)**2+(y1-y2)**2)

{-
Anonymous Inc. has classified its employees into four categories, and has the
following salary policy:
Class 1:
Class 2 or 3:
Class 4:
$10 per hour for regular hours and no overtime
$7 per hour for regular hours and overtime hours at the rate of
1.5 times regular hours
$5 per hour for regular hours, and overtime hours at the rate of
2.0 times the rate for the regular hours.
Write a Haskell function that takes an employee's classification and regular and
overtime hours and returns the employee's pay.
-}

salPol classy regular overtime = if classy == 1 then 10*regular
                                 else if classy == 2 || classy == 3 then 7*regular+7*1.5*overtime
                                 else if classy == 4 then 5*regular+5*2*overtime
                                 else 0