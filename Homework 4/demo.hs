import Prelude hiding (zipWith,any)
import GHC.Parser.Lexer (xset)

main = do 
    print(sumOdds [1,2,3,4,5])

sumOdds :: [Int] ->  Int
sumOdds [] = 0
sumOdds (x:xs) | isOdd x = x + sumOdds xs
               | otherwise = sumOdds xs

isOdd :: Int -> Bool
isOdd x | mod x 2 == 0 = False 
      | otherwise = True


