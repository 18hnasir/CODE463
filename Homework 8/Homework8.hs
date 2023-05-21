{-
Your Name: Hammadullah Nasir G#01112406
Partner: none
-}

module Homework8 where

import Control.Monad
import Control.Monad.State      -- State, runState, get, put, guard
import Data.Bool (Bool(True))
import Data.Maybe (Maybe(Nothing))

data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)
data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

type Name = String
type FamilyTree = [(Name,Name)]


-- =============================================================================

-- given above.
-- data SnocList a = Lin | Snoc (SnocList a) a deriving (Show, Ord)

instance Eq a => Eq (SnocList a) where
  (==) (Snoc a b) (Snoc c d) = (a == c) && (b == d) --if the tails of the lists are equal and the heads then True otherwise False
  (==) Lin Lin = True --if both are nil/empty
  (==) _ _ = False --any other case is false

instance Functor SnocList where
  fmap :: (a -> b) -> SnocList a -> SnocList b
  fmap f Lin = Lin
  fmap f (Snoc a b) = Snoc (fmap f a) (f b)

snocLast :: SnocList a -> Maybe a
snocLast Lin = Nothing
snocLast (Snoc a b) = Just b

snocProduct :: (Num a) => SnocList a -> a
snocProduct Lin = 1
snocProduct (Snoc a b) = b * snocProduct a 

snocMax :: (Ord a) => SnocList a -> Maybe a
snocMax Lin = Nothing
snocMax (Snoc a b) = snocMaxHelper b a

--Helper function for snocMax
snocMaxHelper :: (Ord a) => a -> SnocList a -> Maybe a
snocMaxHelper cm Lin = Just cm
snocMaxHelper cm (Snoc a b) | b > cm = snocMaxHelper b a
                            | otherwise = snocMaxHelper cm a 

longestSnocSuffix :: (Eq a) => SnocList a -> SnocList a -> SnocList a
longestSnocSuffix Lin Lin = Lin
longestSnocSuffix (Snoc a b) Lin = Lin
longestSnocSuffix Lin (Snoc c d) = Lin
longestSnocSuffix (Snoc a b) (Snoc c d) | b == d = Snoc (longestSnocSuffix a c) b 
                                        | otherwise = longestSnocSuffix a c

snocZip :: SnocList a -> SnocList b -> SnocList (a,b)
snocZip Lin Lin = Lin
snocZip (Snoc a b) Lin = Lin
snocZip Lin (Snoc c d) = Lin
snocZip (Snoc a b) (Snoc c d) = Snoc (snocZip a c) (b, d)

snocify :: [a] -> SnocList a
snocify [] = Lin
snocify (x:xs) = Snoc (snocify (init (x:xs))) (last (x:xs)) -- [1,2,3], Snoc (snocify [1,2]) 3 snocify init (x:xs

unSnocify :: SnocList a -> [a]
unSnocify Lin = []
unSnocify (Snoc a b) = unSnocify a ++ [b]

uniques :: SnocList Int -> SnocList Int
uniques Lin = Lin
uniques (Snoc a b) = uniquesHelper (Snoc a b) [] 

--Helper function for uniques, keeps track of already placed values 
uniquesHelper :: SnocList Int -> [Int] -> SnocList Int --snocList currentVals 
uniquesHelper Lin xs = Lin 
uniquesHelper (Snoc a b) [] = Snoc (uniquesHelper a [b]) b --add b to currentVals and make the snockList
uniquesHelper (Snoc a b) cv | elem b cv = uniquesHelper (Snoc a b) cv --if b is an element in currentValues, then ignore
                            | otherwise = Snoc (uniquesHelper a (b:cv)) b 

--Helper function for uniques, checks if element is in the SnocList
isIn :: Int -> SnocList Int -> Bool
isIn x Lin = False
isIn x (Snoc a b) | x == b = True 
                  | otherwise = isIn x a

snocReverse :: SnocList a -> SnocList a
snocReverse Lin = Lin
snocReverse (Snoc a b) = snocify (reverse (unSnocify (Snoc a b)))

-- =============================================================================
 
-- given above.
-- data Tree a = L | V a | Br (Tree a) a (Tree a) deriving (Show)

instance (Eq a) => Eq (Tree a) where
  (==) L L = True
  (==) (V a) (V b) = a == b
  (==) (Br left val right) (Br left2 val2 right2) = (left == left2) && (val == val2) && (right == right2)
  (==) _ _ = False

instance (Ord a) => Ord (Tree a) where
  compare (V a) (V a2) | a < a2 = LT 
                       | a > a2 = GT
                       | a == a2 = EQ 
  compare L (V _) = LT 
  compare (V _) L = GT 
  compare (V _) (Br _ _ _) = LT 
  compare (Br _ _ _) (V _) = GT
  compare (Br l v r) (Br l2 v2 r2) | (l == l2) && (v == v2) && (r == r2) = EQ 
                                   | (v < v2) = LT
                                   | (v > v2) = GT 
                                   | (l < l2) = LT 
                                   | (l > l2) = GT 
                                   | (r < r2) = LT 
                                   | otherwise = GT 



insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree n L = V n --add one value
insertTree n (V a) | n < a || n == a = Br (V n) a L --add left
                   | otherwise = Br L a (V n) --add right
insertTree n (Br l v r) | n < v || n == v = Br (insertTree n l) v r --Making new tree as we go, go left 
                        | otherwise = Br l v (insertTree n r) -- go right

inOrder :: Tree a -> [a]
inOrder L = []
inOrder (V a) = [a]
inOrder (Br l v r) = (inOrder l) ++ [v] ++ (inOrder r)

treeSort :: (Ord a) => [a] -> [a]
treeSort [] = []
treeSort (x:xs) = inOrder (makeTree (x:xs) L)

--Helper function for treeSort, makes the Tree given a list
makeTree :: (Ord a) => [a] -> Tree a -> Tree a
makeTree [] t = t
makeTree (x:xs) t = makeTree xs (insertTree x t)

treeMin :: (Ord a) => Tree a -> Maybe a
treeMin L = Nothing 
treeMin (V a) = Just a 
treeMin (Br l v r) = treeMinHelper v (inOrder (Br l v r)) 

--Helper function for treeMin
treeMinHelper :: (Ord a) => a -> [a] -> Maybe a 
treeMinHelper cm [] = Just cm 
treeMinHelper cm (x:xs) | x < cm = treeMinHelper x xs
                        | otherwise = treeMinHelper cm xs 


-- =============================================================================
-- Maybe Monads

-- useful for some testing; not actualy a required definition, and the
-- tester doesn't import this (it makes its own copy).

family = [
  ("Animal", "Object"),
  ("Cat","Animal"),
  ("Dog","Animal"),
  ("Siamese","Cat"),
  ("Calico","Cat"),
  ("Labrador","Dog"),
  ("Pug","Dog"),
  ("Book","Object"),
  ("Garbage","Can")
  ]

-- given above.
-- type Name = String
-- type FamilyTree = [(Name,Name)]


-- Maybe Monad

parent :: Name -> FamilyTree -> Maybe Name
parent n [] = Nothing 
parent n ((c,f):fs) | n == c = Just f 
                    | otherwise = parent n fs 

ancestors :: Name -> FamilyTree -> Maybe [Name]
ancestors n ft = ancestorsHelper n ft [] 

ancestorsHelper :: Name -> FamilyTree -> [Name] -> Maybe [Name] --name ft currentlist 
ancestorsHelper n ft ns = do 
  family <- parent n ft
  let nns = if isNothing (Just family)  
              then ns --stays the same 
              else ns ++ [family] --add family 
  if isNothing (Just family) 
    then Nothing 
    else if family == "Object"
      then Just nns 
      else ancestorsHelper family ft nns 


--Helper function, checks to see if parent return result is a Nothing 
isNothing :: Maybe Name -> Bool 
isNothing Nothing = True 
isNothing x = False 

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing 
headMaybe xs = Just (head xs)

leastUpperBound :: Name -> Name -> FamilyTree -> Maybe Name
leastUpperBound n1 n2 tree = do
  let name1 = ancestors n1 tree--get list of ancestors for both names
  let name2 = ancestors n2 tree

  if isNothing2 name1 || isNothing2 name2 --if either are empty, then return Nothing
    then Nothing 
    else leastUpperBoundHelper ([n1] ++ justList name1) ([n2] ++ justList name2) --adding the n1 and n2 to the list as well

--Helper function for leastUpperBound, finds the matchin ancestor 
leastUpperBoundHelper :: [Name] -> [Name] -> Maybe Name 
leastUpperBoundHelper [] [] = Nothing 
leastUpperBoundHelper (x:xs) [] = Nothing
leastUpperBoundHelper [] (y:ys) = Nothing  
leastUpperBoundHelper (x:xs) ys | elem x ys = Just x 
                                | otherwise = leastUpperBoundHelper xs ys 

--Helper function, checks to see if list is Nothing
isNothing2 :: Maybe [Name] -> Bool 
isNothing2 Nothing = True 
isNothing2 x = False 

--Helper function, turns Maybe [Name] to [Name]
justList :: Maybe[Name] -> [Name]
justList Nothing = [] 
justList (Just xs) = xs 

-- =============================================================================
-- State Monads

tribM :: Int -> State (Int,Int,Int) Int
tribM 2 = do
  (ut, mt, bt) <- get 
  return ut 
tribM n = do
  (ut, mt, bt) <- get 
  put(bt + mt + ut, ut, mt)
  ans <- tribM (n - 1)
  return ans 

trib :: Int -> Int
trib n = fst $ runState (tribM n) (1,1,1) 

--------------------------------------------------------------------------------

parti :: (a->Bool) -> [a] -> ([a],[a])
parti p [] = ([], [])
parti p xs = partiHelper p xs [] [] 

--Helper Function for parti 
partiHelper :: (a->Bool) -> [a] -> [a] -> [a] -> ([a], [a]) --p values pass fail
partiHelper p [] ps fs = (ps, fs) 
partiHelper p (x:xs) [] [] | p x == True = partiHelper p xs [x] [] 
                           | otherwise = partiHelper p xs [] [x]
partiHelper p (x:xs) ps [] | p x == True = partiHelper p xs (ps ++ [x]) [] 
                           | otherwise = partiHelper p xs ps [x]
partiHelper p (x:xs) [] fs | p x == True = partiHelper p xs [x] fs 
                           | otherwise = partiHelper p xs [] (fs ++ [x]) 
partiHelper p (x:xs) ps fs | p x == True = partiHelper p xs (ps ++ [x]) fs
                           | otherwise = partiHelper p xs ps (fs ++ [x]) 

partitionM :: (a->Bool) -> [a] -> State [a] [a] 
partitionM f xs = do 
  partitionMHelper f xs [] 
  --ps <- get

--Helper function for partitionM 
partitionMHelper :: (a->Bool) -> [a] -> [a] -> State [a] [a] --f values passed 
partitionMHelper f [] fs = do
  return fs  --return the failed values as our answer 
partitionMHelper f (x:xs) fs = do 
  let fail = if f x == False 
                then fs ++ [x] --add value to fail list
                else fs  --keep fail list the same 
  ps <- get --passed list in state
  if f x == True
    then put (ps ++ [x]) --add value to passed list in state
    else return ()

  ans <- partitionMHelper f xs fail 
  return ans 

partition :: (a->Bool) -> [a] -> ([a],[a])
partition f xs = case runState (partitionM f xs) [] of 
                  (a,c) -> (c,a)

--------------------------------------------------------------------------------

simpleBalanced :: String -> Bool 
simpleBalanced "" = True 
simpleBalanced s = simpleBalancedHelper s [] 

--Helper function for the simpleBalanced, considers what the next char is and what actions to do based on that character
simpleBalancedHelper :: String -> [Char] -> Bool --String [what it expects next] 
simpleBalancedHelper [] [] = True 
simpleBalancedHelper [] (es) = False --characters left over mean their was not an even amount 
simpleBalancedHelper (x:xs) [] | x == '(' = simpleBalancedHelper xs [')'] 
                               | x == '{' = simpleBalancedHelper xs ['}']
                               | x == '<' = simpleBalancedHelper xs ['>']
                               | otherwise = simpleBalancedHelper xs [']']
simpleBalancedHelper (x:xs) (es) | x == (head es) = simpleBalancedHelper xs (tail es) --pop the head 
                                 | x == ')' = False --tangled
                                 | x == '}' = False --tangled
                                 | x == '>' = False --tangled
                                 | x == ']' = False --tangled
                                 | x == '(' = simpleBalancedHelper xs (')':es)  --Add to stack 
                                 | x == '{' = simpleBalancedHelper xs ('}':es)  --Add to stack
                                 | x == '<' = simpleBalancedHelper xs ('>':es)  --Add to stack
                                 | x == '[' = simpleBalancedHelper xs (']':es)  --Add to stack
                                 | otherwise = simpleBalancedHelper xs es  --other random character

balancedM :: String -> State [Char] Bool
balancedM [] = do 
  ss <- get
  if ss /= [] 
    then return False 
    else return True 
balancedM (x:xs) = do 
  ss <- get --Get the stack 

  if ss == [] --stack is empty
    then if x == '(' || x == '{' || x == '<' || x == '['
      then put ([x]) --put onto stack 
      else return () --do nothing
    else if x == '(' || x == '{' || x == '<' || x == '[' --stack not empty
      then put (x:ss) --put onto stack 
      else if x == ')' || x == '}' || x == '>' || x == ']' --evaluate if its one of these characters 
        then do 
          let top = head ss 
          if top == '(' && x == ')' 
            then put (tail ss) --pop 
            else return () 
          if top == '{' && x == '}' 
            then put (tail ss) --pop 
            else return () 
          if top == '<' && x == '>' 
            then put (tail ss) --pop 
            else return () 
          if top == '[' && x == ']' 
            then put (tail ss) --pop 
            else return () 
        else return () --some random character, ignore 

  balancedM xs 

balanced :: String -> Bool
balanced s = fst $ runState(balancedM s) [] 

-- =============================================================================
-- List Monads

divisors :: Int -> [Int] --for ans in range(1, 12): if (n % ans) == 0: append
divisors n = do
  ans <- [1..n]
  guard $ mod n ans == 0 
  return ans

geometric :: Int -> Int -> [Int]
geometric n step = do
  ans <- [1..]
  return (n * step^(ans - 1)) --geometric 1 5:  1 * 5^(1 - 1) -> 1 * 5^(2 - 1) -> ...
  

mersennes :: [Int]
mersennes = do
  ans <- [1..]
  return (2^ans - 1)

unitTriangles :: Int -> [(Int,Int,Int)]
unitTriangles n = do
  a <- [1..n]
  b <- [a..n]
  c <- [b..n]
  guard $ a < (b + c) && b < (a + c) && c < (a + b)
  return (a,b,c) 

--------------------------------------------------------------------------------
