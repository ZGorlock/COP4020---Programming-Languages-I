module RegularExpressions where

-- regular expression

infixl 8 <|>
infixl 9 <.>

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "") -- operator section

char :: Char -> RegExp
char c = (== [c])

(<|>) :: RegExp -> RegExp -> RegExp
(<|>) e1 e2 = \s -> e1 s || e2 s 

splits :: [a] -> [([a],[a])]
splits xs = map (flip splitAt xs) [0..length xs]

-- concatenation .
(<.>) :: RegExp -> RegExp -> RegExp
(<.>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<..>) :: RegExp -> RegExp -> RegExp
(<..>) e1 e2 =
  \s -> or [e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon <|> (e <..> star e)

-- extra functions

letter :: RegExp               -- if you put $ then you don't have
                               -- to put parenthesis around ['a' ... 'Z']
letter = foldl1 (<|>) (map char $ ['a'..'z'] ++ ['A'..'Z'])


digit :: RegExp
digit = foldl1 (<|>) (map char ['0'..'9'])

digitNonZero :: RegExp
digitNonZero = foldl1 (<|>) (map char ['1'..'9'])


a = char 'a'
b = char 'b'

-- put your solutions here

-- option

option :: RegExp -> RegExp
option e = epsilon <|> e

-- plus

plus :: RegExp -> RegExp
plus e = e <.> (plus e <|> option e)

-- number

number :: RegExp
number = (digitNonZero <.> star digit) <|> (char '0')

-- fractional number

fractional :: RegExp
fractional = ((char '0') <|> (digitNonZero <.> (star number)))
             <.> char '.' <.>
             ((char '0') <|> ((star number) <.> digitNonZero))