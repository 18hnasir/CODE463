-- Name: Hammadullah Nasir
-- NetID: G01112406


-- please add comments to your code below as usual.


module Homework4 where

import Prelude hiding (zipWith,any)

main = do 
    print(trib 5)
--------------------------------------------------------------------------------

--Function that returns prime factors of n
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n | isPrime n 2 == True = [n]
               | otherwise = getPrimeFactors 2 n

--Helper Function for primeFactors
--Gets the next current prime
getCurrentPrime :: Int -> Int 
getCurrentPrime n | isPrime n 2 == True = n 
                  | otherwise = getCurrentPrime (n + 1)

--Helper Function for primeFactors
--Adds the actual prime factors of the given n
getPrimeFactors ::  Int -> Int -> [Int]
getPrimeFactors cp cn | mod cn cp == 0 && isPrime (div cn cp) 2 = [cp, div cn cp] 
                        | mod cn cp == 0 = cp : continuePrimeFactors1 
                        | otherwise = continuePrimeFactors2
                      where continuePrimeFactors1 = getPrimeFactors cp (div cn cp) --keeps the current prime same but changes current num
                            continuePrimeFactors2 = getPrimeFactors (getCurrentPrime (cp + 1)) cn --changes curretn prime but keeps current num same

--Helper function for primeFactors
--Function to see if a number is prime or not
-- Always pass a 2 for i when calling the function, Ex (isPrime 5 2)
isPrime :: Int -> Int -> Bool 
isPrime n i | n == 1 = True 
            | n == 2 = True 
            | n == i = True
            | mod n i == 0 = False 
            | otherwise = isPrime n (i + 1)


--Helper function for prime factors
--Function to get all factors of a number n
-- Always pass a 1 for i when calling the function, Ex (factors 5 1)
allFactors :: Int -> Int -> [Int]
allFactors n i | n == i = [n]
            | mod n i == 0 = i : factorsContinued
            | otherwise = factorsContinued
            where factorsContinued = allFactors n (i + 1)


--------------------------------------------------------------------------------

--Function that tells if two numbers are coprimes
--Coprime numbers have a greates common divisor of 1
coprime :: Int -> Int -> Bool
coprime a b | gcd a b == 1 = True 
            | otherwise = False

--------------------------------------------------------------------------------

--Function returns the nth tribonnaci number
trib :: Int -> Int
trib n | n < 3 = 1
       | otherwise = getTribNum n 0 1 1 1

--Helper function for trib
--Pass in the value of n, currentsum, bottomThree, middleThree, and upperThree
--Update as the values as funtion is recalled until  n <= 2
getTribNum :: Int -> Int -> Int -> Int -> Int -> Int
getTribNum n cs bt mt ut | n <= 2 = cs
                         | otherwise = getTribNum (n - 1) (bt + mt + ut) mt ut (bt + mt + ut)

--------------------------------------------------------------------------------

--Function finds the max two numbers in a list
maxTwo :: [Int] -> [Int]
maxTwo [] = []
maxTwo [x] = [x]
maxTwo (x:xs) = getTheMaxes (getCurrentMax x xs) (x:xs) --pass in the first cm and the original list

--Helper function for maxTWo
--returns a list with the current max
--If there is a duplicate, it only removes the first occurence
getSecondMaxList :: Int -> [Int] -> [Int]
getSecondMaxList cm [] = []
getSecondMaxList cm (x:xs) | cm == x = xs --If their is an occurence, append the rest of the list
                           | otherwise = x : getSecondMaxList cm xs

--Helper function for maxTwo
--Gets the current max of the given list
getCurrentMax :: Int -> [Int] -> Int 
getCurrentMax cm [] = cm
getCurrentMax cm (x:xs) | cm < x = getCurrentMax x xs --Change the cm
                        | otherwise = getCurrentMax cm xs --cm is not changed

--Helper function for maxTwo
--Returns the list of the two maxes 
getTheMaxes :: Int -> [Int] -> [Int]
getTheMaxes cm (x:xs) = getTheMaxes2 cm (getSecondMaxList cm (x:xs)) --pass the first cm and then the new max list

--Helper function for maxTwo
--Returns the list of the two maxes
getTheMaxes2 :: Int -> [Int] -> [Int]
getTheMaxes2 cm (x:xs) = [cm, getCurrentMax x xs]

--------------------------------------------------------------------------------
--Function that reverses the list
reversed :: [a] -> [a]
reversed [] = []
reversed (x:xs) = reversed xs ++ [x] --append x to the end of the list and repeat until list is reversed

--------------------------------------------------------------------------------

--Function to return a clockwise double list
clockwise :: [[Int]] -> [[Int]]
clockwise [] = []
clockwise (x:xs) = reverseIt (cwHelper1 x xs)

--Helper function for clockwise
--Jumps starts the merging process for a list of length 2 or more
--Or just returns the clockwised version if the list length = 1
cwHelper1 :: [Int] -> [[Int]] -> [[Int]]
cwHelper1 (i:is) [] = cwHelper3 (i:is)
cwHelper1 (i:is) (x:xs) = cwHelper2 (cwHelper3 (i:is)) (x:xs) 

--Helper function for clockwise
--Continues to merge each list together until their are no more
cwHelper2 :: [[Int]] -> [[Int]] -> [[Int]]
cwHelper2 (f:fs) []  = (f:fs)
cwHelper2 (f:fs) (x:xs) = cwHelper2 (merge1 (f:fs) (cwHelper3 x)) xs 

--Helper function for clockwise
--Makes each inner list its own 2D list
--[1, 2, 3] -> [[1], [2], [3]]
cwHelper3 :: [Int] -> [[Int]]
cwHelper3 [] = []
cwHelper3 (i:is) = [i] : cwHelper3 is

--Helper function for clockwise
--Concatenates each innerlists of two lists 
--[[1], [2], [3]] [[4], [5], [6]] -> [[1,4],[2,5],[3,6]]
merge1 :: [[Int]] -> [[Int]] -> [[Int]]
merge1 [] [] = []
merge1 (i:is) (u:us) = merge2 i u : merge1 is us

--Helper function for clockwise
--Concatenates two lists
--[1, 2, 3] [4, 5, 6] -> [1, 2, 3, 4, 5, 6]
merge2 :: [Int] -> [Int] -> [Int]
merge2 [] [] = []
merge2 (i:is) (u:us) = (i:is) ++ (u:us)

--Helper function for clockwise
--Reverses each inner list 
reverseIt :: [[Int]] -> [[Int]]
reverseIt [] = []
reverseIt (x:xs) = reversed x : reverseIt xs

--------------------------------------------------------------------------------

--Function that returns True if any element is True or False otherwise
any :: [Bool] -> Bool
any [] = False
any (x:xs) | x == False = any xs
           | otherwise = True

--------------------------------------------------------------------------------

--Function returns list of passed values for a given predicate
select :: (a->Bool)-> [a] -> [a]
select f [] = []
select f (x:xs) | f x == True = x : select f xs --if the function(x) == True, then add x to list
                | otherwise = select f xs
                              
--------------------------------------------------------------------------------

--Function returns results of applied values to the given function 
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = [] --Covers the edge case of empty lists
zipWith f (x:xs) [] = []
zipWith f [] (y:ys) = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys --apply funtion(x, y) and add result to list

--------------------------------------------------------------------------------

--Function returns a 2D augmented identity matrix
augdentity :: Int -> Int -> [[Int]]
augdentity r c = augHelper 0 r c

--Helper function for augdentity
--Constructs the matrix by making the rows 
--ri = row index, r = #rows, c = #columns
augHelper :: Int -> Int -> Int -> [[Int]]
augHelper ri r c | ri < r = createRow 0 ri c : augHelper (ri + 1) r c
                 | ri == r = []

--Helper function for augdentity
--Creates a row array with the 1 being in the correct position
--i = index (starts off as 0), oi = one index (where the one will be placed)
createRow :: Int -> Int  -> Int -> [Int]
createRow i oi c | i == oi && i < c = 1 : createRow (i + 1) oi c --adds one at correct spot
                        | i == c = []
                        | otherwise = 0 : createRow (i + 1) oi c --adds 0

--------------------------------------------------------------------------------


