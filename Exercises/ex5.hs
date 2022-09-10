import Data.Char
import Data.List
 
{- 1. Write a function that takes a sentence and converts all characters to upper case. -}
 
makeUpper [] = []
makeUpper (x:xs) = [(toUpper x)]++makeUpper xs
 
{- 2. Write a function that takes a list of numbers and then creates another list which
includes pairs with are created by selecting a minimum number and a maximum
number until no number is left in the list. -}
 
removeElem [] _ _ = []
removeElem (x:xs) mini maxi = if x == mini || x == maxi then removeElem xs mini maxi
                              else [x] ++ removeElem xs mini maxi
 
createTuples [] = []
createTuples x = [(minimum x, maximum x)] ++ createTuples (removeElem (x) (maximum x) (minimum x))
 
{- 3. Implement the “capitaliseEachWord” function that takes a sentence to capitalise
the first letter of each word. -}

getEachWord [] = []
getEachWord (x:xs) = [toUpper (head x)] ++ tail x ++ " " ++ getEachWord xs

capitaliseEachWord [] = []
capitaliseEachWord x = getEachWord (words x)