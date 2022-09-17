module Qfour (lastncompare, capitalizeFirstLast, verifier) where

import Data.Char

helper x = "This is a demo helper function hidden from the module"

--Q1)
compareLetters [][] = True
compareLetters (x:xs) (y:ys) = if (x == y || x == (toUpper y) || (toUpper x) == y) then compareLetters xs ys
                               else False

lastncompare [] _ _ = False
lastncompare _ [] _ = False
lastncompare _ _ 0 = True
lastncompare xs ys num = if (num > length xs || num > length ys) then False
                         else compareLetters (drop ((length xs) - num) xs) (drop ((length ys) - num) ys)


--Q2
toUpperFirstLast [y] = [toUpper y]
toUpperFirstLast (x:y:ys) = if (isSeparator x) then [x] ++ [toUpper y] ++ toUpperFirstLast ys
                            else if (isSeparator y) then [toUpper x] ++ [y] ++ [toUpper (head ys)] ++ toUpperFirstLast (tail ys)
                            else [toLower x] ++ toUpperFirstLast (y:ys)

capitalizeFirstLast [] = []
capitalizeFirstLast str = [toUpper (head str)] ++ toUpperFirstLast (tail str)


--Q3
numbers = "0123456789"
countNumbers [] = 0
countNumbers (x:xs) = if (x `elem` numbers) then 1+countNumbers xs
                      else countNumbers xs

lowerCount [] = 0
lowerCount (x:xs) = if (x `elem` ['a'..'z']) then 1+lowerCount xs
                    else lowerCount xs

upperCount [] = 0
upperCount (x:xs) = if (x `elem` ['A'..'Z']) then 1+upperCount xs
                    else upperCount xs

verifier [] = False
verifier str = if ((length str) < 7 || (length str) > 10) then False
               else if ((lowerCount str) >= 2 && (upperCount str) >= 2 && (countNumbers str) >= 1) then True
               else False