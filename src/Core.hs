{-# OPTIONS_GHC -fno-warn-tabs #-}
module Core where
import Control.Applicative
import Data.Char ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper, isPunctuation )
import System.IO () 


data Numeric = I Int | F Float  deriving Show

data Variable = Variable {
        name :: String,
        vtype :: String,
        value :: [[Numeric]] }   --list of list so to represent single numbers, array, matrices es.[[1]], [[1,2]], [[1,2],[3,4]] 
        deriving Show

type Env = [Variable]

newtype Parser a = P (Env -> String -> [(Env, a, String)])    --the parser is a function, wrapped in P

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) = p     --remove the dummy costructor P and does the parsing using the parser p


--parse only the first item of the input
item :: Parser Char
item = P (\env inp -> case inp of
        [] -> []
        (x:xs) -> [(env, x, xs)])

instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap g p = P (\env inp -> case parse p env inp of
                [] -> []
                [(env, v, out)] -> [(env, g v, out)])

instance Applicative Parser where
        -- pure :: a -> Parser a
        pure v = P (\env inp -> [(env, v, inp)])

        -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pg <*> px = P (\env inp -> case parse pg env inp of
                [] -> []
                [(env, g, out)] -> parse (fmap g px) env out)


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= f = P (\env inp -> case parse p env inp of
                [] -> []
                [(env, v, out)] -> parse (f v) env out)

        return a = P (\env cs -> [(env, a, cs)])  --uguale a pure

class Applicative f => Alternative f where
         empty :: f a
         (<|>) :: f a -> f a -> f a
         many :: f a -> f [a]
         some :: f a -> f [a]
         many x = Core.some x Core.<|> pure []
         some x = (:) <$> x <*> Core.many x    --concatenate 1 or more elements x:xs or x:[]

instance Core.Alternative Parser where
        -- empty :: Parser a
        empty = P (\env inp -> [])
        -- (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\env inp -> case parse p env inp of
                [] -> parse q  env inp
                [(envout, v, out)] -> [(envout, v, out)])


-- In combination with sequencing and choice, we define other useful parser. 

 --  parser for single characters that satisfy the predicate p
satisfy :: (Char -> Bool) -> Parser Char        
satisfy p = do { x <- item;
        if p x then return x else Core.empty;}

-- we define parsers for single digits, lower-case letters, upper-case letters,
--  arbitrary letters, alphanumeric characters, and specific characters
digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = do satisfy isAlphaNum 

char :: Char -> Parser Char
char x = satisfy (== x)       -- es. parse (char 'a') [] "abc"  produces [([], 'a', "bc")]

-- using char we can define a parser for the string of char, with the string itself returned as the result value
string :: String -> Parser String
string [] = return []
string (x:xs) = do {
        char x;
        string xs;
        return (x:xs);
        }

-- Using 'many' and 'some', we can now define parsers for identifiers (variable names)
--  comprising a lower-case letter followed by zero or more alphanumeric characters,
--  numbers comprising one or more digits, and spacing comprising zero or more space, tab, and newline char

ident :: Parser String           --es. parse ident "abc def" produces [("abc"," def")]
ident = do {
        x <- lower;
        xs <- Core.many alphanum;
        return (x:xs);
        }

nat :: Parser Int             --es. parse nat "123 abc" --> [(123," abc")]
nat = do {
        xs <- Core.some digit;
        return (read xs);
        }

space :: Parser ()            --es. parse space " abc" --> [((),"abc")]
space = do {
        Core.many (satisfy isSpace);
        return ();
        }

int :: Parser Int          --es. parse int "-123 abc" --> [(-123," abc")]
int = do{
          char '-';
          n <- nat;     --nat return the number if the parse is ok
          return (-n);
        }
	Core.<|>
	 do{
           char '+';
           nat         --same as 'return nat'
	 }
    Core.<|> nat

-- parser for floating point numbers eg. 13.9
numberFloat :: Parser Float
numberFloat = do{
                numbersBeforeComma <- Core.many digit;
                char '.';
                numbersAfterComma <- Core.many digit;
                return (read (numbersBeforeComma++"."++numbersAfterComma))
                }

-- parser for positive and negative floating point numbers eg. +13.9, -2.34
numberFloatWithSign :: Parser Float
numberFloatWithSign = do {
  symbol "+";
  numberFloat;
 } Core.<|> do {
  symbol "-";
  num <- numberFloat;
  return (-num);
 } Core.<|> numberFloat;

--we define token so to define parsers that ignore spaces around identifiers, natural numbers, integers, and special symbols
token :: Parser a -> Parser a
token p = do {
        space;
        v <- p;
        space;
        return v;
        }

identifier :: Parser String
identifier = token ident

integer :: Parser Int
integer = token int

float :: Parser Float
float = token numberFloatWithSign

symbol :: String -> Parser String
symbol xs = token (string xs)


--utility functions for type management and output formatting 

numericToInt:: Numeric ->  Int 
numericToInt (I x) =  x
numericToInt (F x) = round x 

numericToFloat:: Numeric -> Float  
numericToFloat (F x) = x
numericToFloat (I x) = fromIntegral x

--check if a float is an integer e.g. 17.0
checkInt :: Float -> Bool              
checkInt n = floor n == ceiling n  

--print int and float. Float ending with .0 will be printed as int
numericToString :: Numeric -> String 
numericToString (F x) = if checkInt (numericToFloat (F x)) then show (numericToInt (F x)) else show (numericToFloat (F x)) 
numericToString (I x) = show (numericToInt (I x))

--format array to print it
arrayNumericToString:: [Numeric] ->  String  
arrayNumericToString [] = "]"       --if it is the last recursion step
arrayNumericToString (x:xs) | checkInt (numericToFloat x) =  if null xs then numericToString x ++ "" ++ arrayNumericToString xs else numericToString x ++ ", " ++ arrayNumericToString xs
                            | otherwise = if null xs then numericToString x ++ "" ++ arrayNumericToString xs else numericToString x ++ ", " ++ arrayNumericToString xs

--format matrix to print it
matrixNumericToString:: [[Numeric]] ->  String  
matrixNumericToString [] = []
matrixNumericToString (x:xs) = "[" ++ if null xs then arrayNumericToString x ++ "" ++ matrixNumericToString xs else arrayNumericToString x ++ "," ++ matrixNumericToString xs
