toDigits :: Integer -> [Integer]
toDigits 0 = []
--toDigits x = mod x 10 : toDigits(div x 10) --reverse thing
toDigits x = toDigits(div x 10) ++ [mod x 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x:(2*y):doubleEveryOther xs

reverseDoubleEveryOther = reverse . doubleEveryOther . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits(x)) + sumDigits(xs)

--validate :: Integer -> Bool
validate x = 0 == (mod (sumDigits(reverseDoubleEveryOther(toDigits(x)))) 10)

type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b a c
