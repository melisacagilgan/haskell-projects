{-1. Write a Haskell function that takes a list and a number and replicate the elements
of a list a given number of times [3].-}

repli [] _ = []
repli (x:xs) y = take y (repeat x) ++ repli xs y

{-2. Write a Haskell function that takes a list and eliminate consecutive duplicates of
list elements. If a list contains repeated elements they should be replaced with a
single copy of the element. The order of the elements should not be changed [3].-}

compress [] = []
compress [x] = [x]
compress (x:xs) = if x /= (head xs) then [x] ++ compress xs
                  else compress xs

{-3. Implement first and second exercise using lambda abstraction.-}

repli2 a b = (\x y -> [a | a <- x, _ <- [1..y]]) a b

repli3 [] _ = []
repli3 a b = (\(x:xs) y -> take y (repeat x) ++ repli3 xs y) a b


compress2 [] = []
compress2 [x] = [x]
compress2 a = (\(x:xs) -> if x /= (head xs) then [x] ++ compress xs
                          else compress xs) a

{-4. Define a new type for ThreeDShapes. It should have a data constructor for Cube
and Cylinder. Cube should have side length and Cylinder should have radius
and height. You need to implement two functions related to this type.
• volume function calculates the volume of the 3D shape
• surfaceArea function calculates the surface area of the 3D shape.-}

data ThreeDShapes int = Cube  int | Cylinder int int deriving (Show, Eq, Ord)

volume (Cube x)= x*x*x
volume (Cylinder r h) = 3.14*r*r*h 

surfaceArea (Cube x) = 6*x*x
surfaceArea (Cylinder r h) = 2*3.14*r*h + 2*3.14*r

{-5. Modify your data type in exercise four so that it works with both integers and
floating-point numbers.
For question 6,7,8 and 9 you can use the given Tree example and insertElement
function or you can implement your own Tree type and use it.-}

data ThreeDShapes2 a = Cube2  a | Cylinder2 a a deriving (Show, Eq, Ord)

volume2 (Cube2 x)= x*x*x
volume2 (Cylinder2 r h) = 3.14*r*r*h 

surfaceArea2 (Cube2 x) = 6*x*x
surfaceArea2 (Cylinder2 r h) = 2*3.14*r*h + 2*3.14*r

{-6. Write a Haskell function which takes a list of numbers and generates a binary tree.
Hint: You can use the insertElement function-}

data Tree = EmptyTree | Node Int Tree Tree deriving (Show, Eq, Ord)

insertElement x EmptyTree = Node x EmptyTree EmptyTree
insertElement x (Node a left right) = if x == a then (Node x left right)
                                      else if x < a then (Node a (insertElement x left) right)
                                      else  (Node a left (insertElement x right))

inserter [x] = insertElement x EmptyTree
inserter (x:xs) = insertElement x (inserter xs)

{-7. Write a Haskell function which returns the minimum value in the given Tree.-}

minOf (Node a EmptyTree _) = a
minOf (Node a left right) = (minOf left)

{-8. Write a Haskell function that checks if a given Tree is empty or not-}

isEmpty EmptyTree = True
isEmpty (Node a left right) = False

{-9. Write a Haskell function that searches a given element inside a given Tree. It
should return True if the element is found.-}

searchElement x EmptyTree = False
searchElement x (Node a left right) = if x==a then True
                                      else if x<a then (searchElement x left)
                                      else (searchElement x right)