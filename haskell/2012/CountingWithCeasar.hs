module Main where

type Numeral = String

type Thousands = Int
type Hundreds = Int
type Tens = Int
type Ones = Int

-- just split into base 10 digits
data Roman10 = Roman10 Thousands Hundreds Tens Ones
              deriving (Show, Eq)

numerals :: Roman10 -> [Numeral]
numerals (Roman10 n1000 n100 n10 n1) = 
    [ numeralThousands n1000
    , numeralHundreds n100
    , numeralTens n10
    , numeralOnes n1
    ]

roman10 :: Int -> Roman10
roman10 n =
    let (thousands, rem) = splitNum n 1000
        (hundreds, rem') = splitNum rem 100
        (tens, ones) = splitNum rem' 10
    in Roman10 thousands hundreds tens ones

splitNum :: Int -> Int -> (Int, Int)
splitNum n base = (n `div` base, n `rem` base)

romanNumerals :: Int -> Numeral
romanNumerals = concat . numerals . roman10

numeralThousands :: Thousands -> Numeral
numeralThousands n1000 = replicate n1000 'M'

-- it can only have 0-9
numeralHundreds :: Hundreds -> Numeral
numeralHundreds 9 = "CM"
numeralHundreds 8 = "DCCC"
numeralHundreds 7 = "DCC"
numeralHundreds 6 = "DC"
numeralHundreds 5 = "D"
numeralHundreds 4 = "CD"
numeralHundreds n = replicate n 'C'

numeralTens :: Tens -> Numeral
numeralTens 9 = "XC"
numeralTens 8 = "LXXX"
numeralTens 7 = "LXX"
numeralTens 6 = "LX"
numeralTens 5 = "L"
numeralTens 4 = "XL"
numeralTens n = replicate n 'X'

numeralOnes :: Ones -> Numeral
numeralOnes 9 = "IX"
numeralOnes 8 = "VIII"
numeralOnes 7 = "VII"
numeralOnes 6 = "VI"
numeralOnes 5 = "V"
numeralOnes 4 = "IV"
numeralOnes n = replicate n 'I'


main :: IO ()
main = do
    contents <- getContents
    let ns = map read $ lines contents
        numerals = map romanNumerals ns
    mapM_ print numerals
