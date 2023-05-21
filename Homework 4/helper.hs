-- make it a module so you can import it to other places.
module HelloHaskell where

-- we will hide any built-in definitions that we redefined for
-- ourselves below, to avoid name clashes.
import Prelude hiding (take,drop,repeat,cycle,sum,dropWhile,foldr, foldl,
                       map, zip, filter)

-- load this file with:  ghci HelloHaskell.hs
-- you can reload the file interactively with either   :r    or   :reload 


-- single-line comments start with a double-dash and go to the end of
-- the line. At least a space afterwards is polite.
---but not required.

--------------------------------------------------------------------------------

{- multi-line comments are started with {-, and they end with -}. What's more,
they can be *NESTED*! -}

-- identifiers can use letters, numbers, underscores, and apostrophes.

-- names can refer to one value.
n5 = 5

hi = "hi!"

-- when defining a function, write an equation (or many).
inc x = x+1

-- double :: Int -> Int
double x = x*2


-- two patterns are provided to fully define the choose function.
myAnd :: Bool -> Bool -> Bool
myAnd True  True  = True
myAnd True  False = False
myAnd False True  = False
myAnd False False = False

-- note the "dont-care" underscore, which pattern-matches one anything,
-- and then you can't use it in the answer.
anotherAnd :: Bool -> Bool -> Bool
anotherAnd True True = True
anotherAnd _ _ = False


choose :: Bool -> Int -> Int -> Int
choose True  a b = a
choose False a b = b

choosev2 guard e1 e2 = if guard then e1 else e2

-- interactively, feed up to all allowed arguments by putting them
-- adjacent to the function:
e1 = inc 5
e2 = choose True 100 0

-- partially applied: allowed, but function values can't be
-- printed... so trying to show this one will fail, but we can use it
-- next.
e3 = choose False
e4 = e3 2 4        -- it's like we'd said "choose False 2 4".

--------------------------------------------------------------------------------

-- we have if-expressions.

max2 a b = if a>b
           then a
           else b

-- but we can use "pattern guards" to avoid if-expression mania.
message x | x<0       = "negative"
          | x==0      = "zero"
          | otherwise = "positive"


max3 :: Int -> Int -> Int -> Int
max3 a b c | a>=b && a>=c = a
           | b>=c         = b
           | otherwise    = c

-- alternate definition:
max3' a b c = max2 a (max2 b c)


-- negative numbers often need to be parenthesized, since we use
-- adjancency to feed arguments to a function. The dash looks like
-- subtraction to Haskell.

ex5 = message 5
ex6 = message (-3)



-- converting numbers: the functions round, floor, ceiling help you convert
-- floating points to Integral numbers (i.e., from a Double to an Int).

needsInt :: Int -> Int
needsInt x = x+1

needs_ex1 = needsInt (round 5.2)

-- converting from an Int to a Double, you use the fromIntegral function, but
-- you can already use an Int where a Double is needed.
needsDouble :: Double -> Double
needsDouble x = x + 0.25

needs_ex2 = needsDouble 3          -- succeeds, because 3 can be a Double

someInt = 3 :: Int

-- needs_ex3 = needsDouble someInt -- fails, because Int isn't auto-cast.

needs_ex4 = needsDouble (fromIntegral someInt)


--------------------------------------------------------------------------------

-- Lists! They can be written e.g. [2,4,6,8], but really there are two
-- kinds of lists: an empty list, and a list that has a front item
-- (the "head" item) and the rest of the list (some other list value).

emptylist = []

-- we can build a non-empty list with the "cons" operator, an infix
-- colon between the head-item and the tail-list.
singleton = 1 : []
a_few_values = 1 : ( 2 : (3 : []))

-- we get the same effect with the "syntactic sugar" of comma-separated lists.
a_few_values' = [1, 2, 3]


-- to write a function on lists, pattern-match the two kinds (empty,
-- non-empty), or also syntactic sugar versions.
penultimate :: [a] -> a
penultimate []     = error "not enough items to find the second-to-last!"
penultimate [a]    = error "one isn't enough."
penultimate [a,b]  = a
penultimate (x:xs) = penultimate xs

{- Notes:

   - all four equations, taken together, makes the definition of the
     function penultimate.
   - the function accepts a single argument, a list.
   - the first pattern that matches is used.
   - variables (like a, b, x, xs) will match whatever stuff is there;
     the [], :, and syntactic sugar[,,,,] are the actual structural
     pieces that must be found for the pattern-match to succeed.
-}

ex7 = penultimate [1,2,3,4,5]
-- we can define ranges of values with the dot-dot syntax:
ex8 = penultimate [1..100]
-- and we can have a step value other than +1.
ex9 = penultimate [1,3..100]

-- in fact, let's just never stop the party. here's an infinite list.
naturals = [1..]

-- how about an inexhaustible supply of some value?
repeat v = v : repeat v

-- or an inexhaustible supply of some list of values?  ( ++ is the
-- list concatenator, which combines two lists into a new list).
cycle xs = xs ++ (cycle xs)

-- let's get some starting portion of the infinite list (or any list).

take n xs | n<=0 = []
take n (x:xs)    = x : (take (n-1) xs)
-- silently give less than n items when we run out.
take n [] = []

summ :: [Int] -> Int
summ (x:rest) = x + summ rest
summ [] = 0




sum [] = 0
sum (x:rest) = x + sum rest


--------------------------------------------------------------------------------

-- tuples are things like pairs, triplets, etc. I think haskell
-- provides up to size-64-tuples, but for your sanity use just short
-- ones. The different slots can each be different types.

ex15 = (4,5)
ex16 = (True,[1,2,3], "hello!", 3.5)

-- we often make the unrolled-loop recursive function as a helper
-- (like minmaxHelper), and then wrap that in a "driver" function
-- (like minmax).

minmax :: [Int] -> (Int,Int)
minmax []     = error "no items!"
minmax (x:xs) = minmaxHelper x x xs

minmaxHelper :: Int -> Int -> [Int] -> (Int,Int)
minmaxHelper low high []    = (low,high)
minmaxHelper low high (x:xs) | x<low     = minmaxHelper  x  high xs
                             | x>high    = minmaxHelper low  x   xs
                             | otherwise = minmaxHelper low high xs



ex17 = minmax [1,32,4,6,77,4,2,45,56,68,8,-34,456,57,5,34,3,-45,56,76,53,43,4,-3]


-- turn two lists into a list of pairs. Give up as soon as either list
-- is exhausted.
pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] [] = []
pairUp xs [] = []
pairUp [] ys = []
pairUp (x:xs) (y:ys) = (x,y) : (pairUp xs ys)


-- oh wait, pairUp is just the built-in zip! And here's an alternate
-- defintion, using the "don't-care" underscore, which matches anything
-- but can't be used on the righthand side.

pairUp' :: [a] -> [b] -> [(a,b)]
pairUp' (x:xs) (y:ys) = (x,y) : (pairUp' xs ys)
pairUp' _      _      = []

ex18 = pairUp [1,2,3,4,5] "ABCDEFGHIJKLMNOP"

-- you can use functions as first class values, so you can even do this and
-- entirely borrow the definition!
pairUp'' = zip



--------------------------------------------------------------------------------

-- functions are first-class values. This means they can be passed
-- around, fed as arguments, and be anywhere other values (like
-- integers, strings, etc) would be allowed.

twice :: (a->a) -> a -> a
twice f x = f (f x)


-- we can define anything locally with let-expressions. It helps with
-- getting a bit of an imperative feel, but try not to overdo it (your
-- code will start smelling bad). Here's a gratuitous example.
ex19 = (let
           five    = 5
           plus5 x = x + five
        in
           twice plus5 100
       )

-- another function-argument example.
each :: (a->b) -> [a] -> [b]
each f []     = []
each f (x:xs) = (f x) : (each f xs)

-- oh wait, each is just the built-in map!
--
-- map f []     = []
-- map f (x:xs) = (f x) : (map f xs)

ex20 = each inc [1,2,3]

isEven :: Int -> Bool
isEven x = (mod x 2 == 0)

ex21 = each isEven [1,2,3,4,5]

pairs = [(1,2),(3,4),(5,6),(1,1),(2,14)]


firstTrio (a:b:c:[]) = [a,b,c]


-- pattern-matching on triplets. Oddly missing functions.
fst3 (a,_,_) = a
snd3 (_,b,_) = b
third3(_,_,c) = c

nth n [] = error "ran out!"
nth 0 (x:xs) = x
nth n (x:xs) = nth (n-1) xs


firstsOf3s [] = []
firstsOf3s ((a,b,c):rest) = a : (firstsOf3s rest)


--------------------------------------------------------------------------------

-- We don't have to name a function if we only need it in one place -
-- "anonymous functions", or "lambdas", are built right into the
-- language. Let's start using map and zip instead of our each and
-- pairUp functions so we get used to seeing them.

ex22 = map (\ x -> x*10) [1,2,3,4]

ex23 = map (\ (a,b) -> a+b ) (zip [1,2,3] [10,10,10,10])


-- we can get carried away with them - let's remind ourselves that
-- *all* function definitions can be thought of as one-argument
-- functions that may in turn result in another function that accepts
-- more arguments...
plus3 :: Int -> (Int -> (Int -> Int))
plus3 a b c = a+b+c

add3 = \ x -> \ y -> \ z -> x+y+z

-- same, with explicit parentheses:
add3' = \ x -> (\ y -> (\ z -> x+y+z))

-- same behavior, but showing LHS params vs explicit lambda for 3rd param (z).
add3'' x y = \z -> x+y+z

-- and you can partially apply any function by just giving it some of
-- its arguments...


-- NOTE: gratuitous scrambling, reorganization, and obfuscation of code!
ex24 = let
           first  = add3'' 5
           third  = second 15
           second = first 10
       in
           third   -- same effect as (add3'' 5 10 15)


ex25 = map (\x -> x*5) [1,2,3,4,5]
ex26 = map (\x -> x*5) [10,20..100]


-- "sectionals" let us put one argument next to a binary infix operator.
ex27 = map (*5) [1,2,3,4,5]

ex28 = map (10-) [1,2,3,4,5]



-- abstract data types are one of the biggest new things you may
-- encounter in Haskell. Instead of an inheritance hierarchy of
-- loosely-related child classes, we define all the different possible
-- shapes of some type of values in one place.

-- each "constructor" is some new identifier, followed by the types of
-- any arguments needed to build up the value. (Sometimes there are no
-- arguments). These different forms are separated by pipes |.

-- for now, ignore the deriving clause. (it's giving us .toString()
-- and .equals() functionality, in the Java sense).

data Coin = Penny | Nickel | Dime | Quarter                deriving (Show, Eq,Ord)
data Grade = Score Int | Excuse String | Absent            deriving (Show, Eq)
data Color = Red | Orange | Blue | Gray | RGB Int Int Int  deriving (Show, Eq)

countMoney :: [Coin] -> Int
countMoney [] = 0
countMoney (Quarter:rest) = 25 + countMoney rest
countMoney (Dime   :rest) = 10 + countMoney rest
countMoney (Nickel :rest) =  5 + countMoney rest
countMoney (Penny  :rest) =  1 + countMoney rest

cm :: [Coin] -> Int
cm [] = 0
cm (c:cs) =
  case c of
    Quarter -> 25 + cm cs
    Dime    -> 10 + cm cs
    Nickel  -> 5  + cm cs
    Penny   -> 1  + cm cs

cm' :: [Coin] -> Int
cm'  [] = 0
cm' (c:cs) = (let val Quarter = 25
                  val Dime    = 10
                  val Nickel  = 5
                  val Penny   = 1
             in val c) + (cm' cs)


cm'' [] = 0
cm'' (x:xs) | x==Quarter = 25+ cm'' xs

-- sometimes a helper is just provided by a let-expression.
avgGrades :: [Grade] -> Int
avgGrades gs = let
   sumGrades :: [Grade] -> Int
   sumGrades [] = 0
   sumGrades ((Score n ): rest ) = n + sumGrades rest
   sumGrades ((Excuse _): rest ) = 0 + sumGrades rest
   sumGrades ((Absent  ): rest ) = 0 + sumGrades rest

   numGradable :: [Grade] -> Int
   numGradable [] = 0
   numGradable (Score  _ :rest) = 1 + numGradable rest
   numGradable (Excuse _ :rest) = 0 + numGradable rest
   numGradable (Absent   :rest) = 1 + numGradable rest -- no makeups! >:E

   total :: Int
   total = sumGrades gs

   count :: Int
   count = numGradable gs
   in
    div total count  -- div is integral division.


results = [Score 100, Score 95, Excuse "a dog ate my homework", Excuse "plz", Absent, Score 125]



{--

-- the real optional-value ADT is called Maybe:

data Maybe a = Just a | Nothing

-- we could make our own like this, but let's just use the real Maybe.
data Mayhaps a  = Have a | Dont deriving (Show, Eq)


--}


maxVal :: [Int] -> Maybe Int
maxVal []      = Nothing
maxVal (x:xs)  = case (maxVal xs) of
                  Nothing  -> Just x
                  Just ans -> Just (if x>ans then x else ans)
                    
showMaxVal xs = case maxVal xs of
  Nothing -> "nothing"
  Just x  -> "max is " ++ (show x)


{--
-- the real one is called Either:
data Either a b = Left a | Right b   

-- we could make our own, but won't:
data Crashy a = Bad String | Good a deriving (Show, Eq)
--}

divide :: Int -> Int -> Either String Int
divide a b | b==0      = Left "can't divide by zero."
           | otherwise = Right (div a b)

--------------------------------------------------------------------------------

-- more examples of pattern-matching on ADTs:

-- extract the actual values, discard all Nones and the explicit Just-labels.
justs :: [Maybe a] -> [a]
justs (Nothing:xs)  =   justs xs
justs ((Just x):xs) = x:justs xs
justs [] = []

-- try calling justs on each of these.
mays1 = [Nothing, Just 3, Just 5, Nothing, Just 100, Nothing]
mays2 = [Nothing,Nothing,Nothing]

-- alternate definition that uses a case expression; I prefer the
-- above formulation but getting used to case expressions is valuable.
justs' [] = []
justs' (x:xs) = case x of
                 Nothing ->     justs' xs
                 Just v  -> v : justs' xs

-- like a two-level justs operation that also adds up the eventual values.
collapse :: [Maybe (Maybe Int)] -> Int
collapse [] = 0
collapse (Nothing        : xs) = collapse xs
collapse ((Just Nothing) : xs) = collapse xs
collapse ((Just (Just x)): xs) = x + collapse xs

-- shorter definition!
collapse' xs = sum (justs (justs xs))

--crazily short: uses function composition (.); (.) is described later on.
collapse'' :: [Maybe(Maybe Int)] -> Int
collapse'' = sum . justs . justs


maymay1 :: [ Maybe (Maybe Int) ]
maymay1 = [Just (Just 0), Nothing, Just (Just 1), Just Nothing, Just (Just 2), Nothing]

--maymay2 :: [ Maybe (Maybe Bool) ]
maymay2 = [Just (Just True), Nothing, Just (Just False), Just Nothing, Just (Just False), Nothing]



think :: [Either Int String] -> Int
think [] = 0
think ((Left  n) : xs) = n + think xs
think ((Right s) : xs) = length s + think xs

eis = [Left (3::Int), Left 6, Right "hello"]

leftint = Left (3::Int)

--------------------------------------------------------------------------------

-- pattern matching can go as deep as you want. Here we have a list of
-- pairs, and we can ask for a lot of structure in our pattern
-- matches.
countUps [] = 0
countUps ((a,b):pairs) | a<b       = 1 + (countUps pairs)
                       | otherwise = countUps pairs


-- pattern-matching is useful at the top level, but it's also
-- useful... everywhere. We can use case-expressions anywhere else we
-- want pattern-matching.


dropWhile f []     = []
dropWhile f (x:xs) = case (f x) of
                        True  -> dropWhile f xs
                        False -> x:xs


--------------------------------------------------------------------------------

-- TYPES! everything above is strongly typed, and has its specific
-- type. We can ask the interpreter for any expression's type with:
--
--      HelloHaskell>  :t  True
--      Bool
--
-- some are quite general, e.g. our map and zip re-implementations:
--
--      HelloHaskell>  :t map
--      (a -> b) -> [a] -> [b]
--
--      HelloHaskell>  :t zip
--      [a] -> [b] -> [(a, b)]
--

-- we can explicitly give a type signature, which might constrain the
-- definition more than the general type Haskell would find (limiting
-- where we can use it but also limiting misuse/bugs/unexpected usage).

mapInts :: (Int -> Int) -> [Int] -> [Int]
mapInts f xs = map f xs

--------------------------------------------------------------------------------

-- list comprehensions: sort of a set-notation:

-- { x^2 | x elt. Z+ }
ex29 = [ x^2 | x <- [1..] ]

-- we can have multiple "generators":
ex30 = [(a,b) | a<-[1..4], b<-[1..4]]

{-
in python:
ans = []
for a in [1,2,3,4]:
  for b in [1,2,3,4]:
    ans.append((a,b))

-}

-- we can include constraints:
ex31 = [(a,b) | a<-[1..4], b <- [1..4], a<b]

-- right triangles with side lengths up to n:
tris n = [(a,b,c) | a<-[1..n]
                  , b<-[1..n]
                  , a<=b
                  , c<-[1..n]
                  , b<=c
                  , (a^2+b^2)==c^2
                  ]
{-
ans = []
for a in range(1,n+1):
  for b in range(1,n+1):
    if not (a<=b):
      continue
    for c in range(1,n+1):
      if not (b<=c):
        continue
      if not (a**2+b**2 == c**2):
        continue
      ans.append((a,b,c))

-}




ex32 = tris 20

-- you can nest list comprehensions, but it gets ugly!
-- a list comprehension builds a list, so it can be used as a generator.

ex33 = [ (i,j) | i<-[x|x<-[1..10]], j<-[i..10] ]

{-
sort of like nested for-loops:
xs = []
for i in range(1,10+1):
  for j in range(i,10+1):
    xs.append((i,j))
-}

--------------------------------------------------------------------------------

-- function composition: f • g. functions that take one argument each
-- can be chained together to indicate f(g(h(x)) using the
-- dot-operator: f . g . h

-- recall inc, already in this file above:
-- inc x = x+1

mul2   x = x*2
square x = x*x

-- go(x) == square(mul2(inc(x)))
go :: Int -> Int
go = square . mul2 . inc

ex34 = go 4
ex35 = go 5


-- application syntax: use the $ operator to separate a function from
-- its argument. All we get is different precedence, but we can avoid
-- a lot of parentheses. $ is right-associative, and low precedence
-- (grabs as much as it can after other things).

-- these are equivalent:
max6  a b c d e f = max3 a b ( max3 c d ( max e   f ))
max6' a b c d e f = max3 a b $ max3 c d $ max e $ f

-- combining the two is pretty useful, often looking something like:
-- name = expr . expr . expr $ expr
ex36 = map (\(n,cs)->take n cs)
     . zip [1..10]
     . map repeat
     $ "abcdefghijklmnop"


-- any time a definition looks like func arg = expr $ arg, you can
-- remove the arg:

getEvennesses  :: [Int] -> [Bool]
getEvennesses xs = map even xs

getEvennesses' :: [Int] -> [Bool]
getEvennesses'   = map even


-- but no-argument functions in Haskell sometimes are best served with
-- a type ascription.


--------------------------------------------------------------------------------

-- some higher-order functions you should learn about (all hidden from
-- the Prelude for this document):



-- output preserves the length of the input list, but can change each value.
map     :: (a->b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs



-- output may have fewer items than input list, but does not modify any values
-- it keeps.
filter  :: (a->Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x       = x : filter f xs
                | otherwise =     filter f xs

-- output length is based on the inputs' length, and other than pairing values
-- up does not change them.
zip     :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _       _     = []


{--

sketch of zip usage (evaluation steps):

let's evaluate this expression:

    zip [1,2,3] [True, True, False, False]

remind ourselves that we can pattern-match e.g. so that [1,2,3] is (1:[2,3]).
(We might even immediately jump to (1:(2:(3:[]))), but I didn't do so here).

see the noted names from our pattern-match of (x:xs) (y:ys).

    zip (1:[2,3]) (True : [True, False, False])
         x -xs--    -y--   -------ys-----------

The RHS of our chosen zip definition line is (x,y):zip xs ys.
Using the actual things in our expression, we get:

    (1,True) : (zip [2,3] [True, False, False] )

Now we are only looking at that inner zip call, but the (1,True) is still part
of our overall answer, so we leave it in place as we are simplifying the whole
original call to a value.

    (1,True) : (zip (2:(3:[])) (True: [False, False]))
                     x  -xs-    -y--  ------ys------

same idea again, with shorter argument lists...

    (1,True) :(2,True) : (zip (3:[]) (False:[False] )

once again...

    (1,True) :(2,True) : (3,False) : (zip [] (False:[]) )


This time, our zip call doesn't match the two-nonempty-lists pattern, so we use
the second base-ccase one: zip _ _ = [].

    (1,True) :(2,True) : (3,False) : []

same answer, but with pretty-printed syntax:

[(1, True), (2, True), (3, False)]

--}



{-- combining these can be useful. Given two lists of ints, let's return the
    values from the first list that were larger than the same-index values in
    the second list, squaring each one.
--}

biggerSquares xs ys =  map (^2) $ map fst $ filter (\(x,y) -> x>y)  $ zip xs ys
-- biggerSquares [1,5,10,6,3,8,10] [4,4,4,4,4,4,4]

-- the all in one manual approach:
comp1 (x:xs) (y:ys) | x>y       = (x^2) : comp1 xs ys
                    | otherwise =         comp1 xs ys
comp1 [] (y:ys)  = []
comp1 (x:xs)  [] = []
comp1 [][]       = []


-- the learning-to-use-hof's approach:
comp2 xs ys = let pairs   = zip xs ys
                  biggers = filter (\(x,y) -> x > y) pairs
                  bigs    = map fst biggers
              in
                  map (\x->x^2) bigs


--------------------------------------------------------------------------------

-- consuming a list with a binary operator: a.k.a. "reducing" or "folding"
-- the standard names are: foldl, foldr, foldl', etc.


-- folding a list (or other structure) means to collapse it down to a
-- single value. An example would be to collapse a list of ints down
-- to their sum, or to the maximum value.


-- righthand version easily fills the stack with 1+(2+(3+(....)))
foldr :: (a->b->b)->b->[a]->b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

{--

really it'd be prefix style, not infix plusses:
   foldr (+) 0 [1,2,3]
    ==   (+) 1 (foldr (+) 0 [2,3])
    ==   (+) 1 ((+) 2 (foldr (+) 0 [3]))
    ==   (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
    ==   (+) 1 ((+) 2 ((+) 3 0))                  ===   (1+(2+(3+0)))
--}


-- lefthand fold still leaves lazy uncalculated portions in the heap,
-- so it *still* can overflow!
foldl :: (r->d->r) -> r -> [d] -> r
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
{--
  lefthand fold still leaves lazy uncalculated portions in the heap,
  so it *still* can overflow!
        foldl (+) 0 [1,2,3]
     == foldl (+) ((+) 0 1) [2,3]
     == foldl (+) ((+) ((+) 0 1) 2) [3]
     == foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []
     ==      ((+) ((+) ((+) 0 1) 2) 3)          === (((0+1)+2)+3)
--}
  


-- we need to ask haskell to force evaluation of the redexes: seq a b
-- forces some evaluation on a, then actually calculates b as its
-- answer; when a is used in both, it'll already be computed when used
-- in b to construct the result. We only use this (seq) when we want to
-- drive evaluation order, which is normally not needed/considered.
foldl' :: (r->d->r) -> r -> [d] -> r
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x
                    in seq z' $ foldl' f z' xs

{--
 a bit of liberty with representation, but here's what happens:
        foldl' (+) 0 [1,3,5]
     == foldl' (+) (seq ((+) 0 1)) [3,5]    - seq forces ((+) 0 1) to eval to 1.
     == foldl' (+)        1        [3,5]    - 
     == foldl' (+) (seq ((+) 1 3)) [5]      - seq forces ((+) 1 3) to eval to 4.
     == foldl' (+)        4        [5]      - 
     == foldl' (+) (seq ((+) 4 5)) []       - seq forces ((+) 4 5) to eval to 9.
     == foldl' (+)        9        []       - 
     ==          9
--}


{-

  Here is a related discussion page on folds:

    https://wiki.haskell.org/Foldr_Foldl_Foldl%27

-}


--------------------------------------------------------------------------------

-- practice

-- some basic recursion
factorial n = if n<=1
              then 1
              else n * (factorial (n-1))

fact' n | n<=1       = 1
        | otherwise  = n * (factorial (n-1))


fact'' 1 = 1
fact'' n = n * (factorial (n-1))




addEvens :: [Int] -> Int
addEvens xs = foldl' (+) 0 . filter even $ xs

-- the "point-free style" involves writing a sequence of
-- transformations on the one argument to a function. This allows us
-- to not name any parameters! But it is often over-kill.

addEvens' :: [Int] -> Int
addEvens' =  foldl' (+) 0 . filter even

-- beginner-style code
addEvens'' :: [Int] -> Int
addEvens'' xs = let boolAnswers = map even xs
                    pairs       = zip boolAnswers xs
                    values      = map (\(b,n)->if b then n else 0) pairs
                    sumList []     = 0
                    sumList (x:xs) = x + sumList xs
                in sumList values



-- building a list
repeatn :: Int -> a -> [a]
repeatn n v = if n>0
              then v : (repeatn (n-1) v)
              else []

--------------------------------------------------------------------------------


-- select a pivot, filter out those lower and higher than the pivot,
--  sort both sides, and glue together.

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = let pivot   = x
                   lowers  = filter (\n -> n <= pivot) xs
                   highers = filter (\n -> n >  pivot) xs
               in (qsort lowers) ++ [pivot] ++ (qsort highers)


{-- general strategies:
     - helper functions
     - driver function (probably fewer arguments) that calls the "worker" function.

     - think of data flow more than control flow:
       - what "milestone" values can you compute?

     - build each new value from scratch, we don't modify existing values.

     - let-expressions allow you to name something and then use it "afterwards"
       (in the "in" block).

     - do you have an Int and need a Double? They are not as directly linked by
       subtyping as in other languages.

       - use fromIntegral.

          *ClassCode> 3.5 / (4::Int)
          <interactive>:8:1: error:
              - No instance for (Fractional Int) arising from a use of ‘/’
              - In the expression: 3.5 / (4 :: Int)
                In an equation for ‘it’: it = 3.5 / (4 :: Int)
          *ClassCode> 
          *ClassCode> 3.5 / (fromIntegral (4::Int))
          0.875

--}


-- a bit contrived of a driver/helper.
mean :: [Double] -> Maybe Double
mean [] = Nothing
mean xs = Just (meanHelper xs (length xs))


meanHelper :: [Double] -> Int -> Double
meanHelper xs len = let total = foldr (+) 0 xs
                    in total / (fromIntegral len)



-- extra state that we needed could be made into extra parameters for the worker.
countFives :: [Int] -> Int
countFives xs = countFivesHelper xs 0

countFivesHelper [] c = c
countFivesHelper (x:xs) c | x==5      = countFivesHelper xs (c+1)
                          | otherwise = countFivesHelper xs  c


-- ...though using higher-order functions will often solve it for us too!
countFives' xs = length (filter (\x->x==5) xs)



{--
-- Example of using let-expression to break a problem down into smaller parts.
-- get the mean
-- square the absolute value of each item-minus-the-mean
-- divide it by N, sqrt that.
-- I'm choosing that stdDev of an empty list is zero. (not sure about it but that's
   not too important for the example.)
--}


stdDev :: [Double] -> Double
stdDev [] = 0.0
stdDev xs = let (Just meanval) = mean xs  -- you'd better be sure it's a "Just"!
                sums           = map (\x -> (abs (x - meanval))**2) xs
                numerator      = sum sums
                amount         = length xs
                fraction       = numerator / (fromIntegral amount)
            in  fraction ** (0.5)

--------------------------------------------------------------------------------