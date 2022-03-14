import Control.Applicative 
import Data.Char
    ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )
import System.IO 
data Variable = Variable {
        name :: String,
        vtype :: String,
        value :: [[Int]] }   --list of list so to represent single int, array, matrices es.[[1]], [[1,2]], [[1,2],[3,4]] 
        deriving Show

type Env = [Variable]

newtype Parser a = P (Env -> String -> [(Env, a, String)])   --the parser is a function, wrapped in P

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp    --remove the dummy costructor P

interpreter :: Env -> String -> String
interpreter env xs = case (parse program env xs) of
        [(env, n, [])] -> showMemoryState env
        [(env, _, out)] -> error ("Unprocessed input string : " ++ out)
        [] -> error "Wrong input"

showMemoryState :: Env -> String
showMemoryState []             = []
showMemoryState (x:xs) = (name x) ++ "=>" ++ ( if (vtype x == "Integer") then show ((value x !! 0) !! 0)    --show the only value of the only list of the list of list
else ( if (vtype x == "Array") then show ((value x) !! 0) else  show (value x))) ++ " " ++ (showMemoryState xs)

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

        many x = Main.some x Main.<|> pure []
        some x = (:) <$> x <*> Main.many x    --concatenate 1 or more elements x:xs or x:[]

instance Main.Alternative Maybe where
        -- empty :: Maybe a
        empty = Nothing

        -- (<|>) :: Maybe a -> Maybe a -> Maybe a
        Nothing <|> my = my
        (Just x) <|> _ = Just x

instance Main.Alternative Parser where
        -- empty :: Parser a
        empty = P (\env inp -> [])
        -- (<|>) :: Parser a -> Parser a -> Parser a
        p <|> q = P (\env inp -> case parse p env inp of
                [] -> parse q  env inp
                [(envout, v, out)] -> [(envout, v, out)])

-- In combination with sequencing and choice, we define other useful parser. 

satisfy :: (Char -> Bool) -> Parser Char         --  parser for single characters that satisfy the predicate p
satisfy p = do { x <- item;
        if p x then return x else Main.empty;}

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
alphanum = satisfy isAlphaNum

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

-- Using many and some, we can now define parsers for identifiers (variable names)
--  comprising a lower-case letter followed by zero or more alphanumeric characters,
--  natural numbers comprising one or more digits, and spacing comprising zero or more space, tab, and newline char

ident :: Parser String           --es. parse ident "abc def" produces [("abc"," def")]
ident = do {
        x <- lower;
        xs <- Main.many alphanum;
        return (x:xs);
        }

nat :: Parser Int             --es. parse nat "123 abc" --> [(123," abc")]
nat = do {
        xs <- Main.some digit;
        return (read xs);
        }

space :: Parser ()            --es. parse space " abc" --> [((),"abc")]
space = do {
        Main.many (satisfy isSpace);
        return ();
        }

int :: Parser Int          --es. parse int "-123 abc" --> [(-123," abc")]
int = do {
                char '-';
                n <- nat;     --nat return the number if the parse is ok
                return (-n);
                }
        Main.<|> nat

--parse the array brackets and (eventual)spaces after them
array :: Parser [Int]
array = do {
                char '[';                   --parse the [
                space;                      --parse 0 o more spaces
                a <- arrayContent;          --parse the array content
                space;
                char ']';
                return a;
        }

--parse the array elements, spaces and commas after them
arrayContent :: Parser [Int]
arrayContent = do {
                a0 <- aexp;       --parse the first element of the array
                space;
                char ',';
                a1 <- arrayContent;    --parse the recursively the other elements
                return (a0 : a1);      -- return the parsed array content
        }
        Main.<|> do{               --in case of an array with only one element
                space;
                a0 <- aexp;
                space;
                return [a0];
        }

--parse the matrix external brackets and (eventual)spaces after them
matrix :: Parser [[Int]]
matrix = do {
                char '[';
                space;
                a <- matrixContent;
                space;
                char ']';
                return a;
        }

--parse all the arrays which composes the matrix
matrixContent :: Parser [[Int]]
matrixContent = do {
                space;
                a0 <- array;      --parse the first list(row) of the 'list of list'(matrix)
                space;
                char ',';
                a1 <- matrixContent;     --parse the other rows, ricursively
                return (a0 : a1);
        }
        Main.<|> do{                    --the matrix has only one row
                space;
                a <- array;
                space;
                return [a];
        }

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

tokenArray :: Parser [Int]
tokenArray = token array

tokenMatrix :: Parser [[Int]]
tokenMatrix = token matrix

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


--enviroment management

-- Return the value of a variable given the name (and given the indeces in case of array or matrix)
readVariable :: String -> Parser Int
readVariable name = P (\env input -> case searchVariable env name of
        [[]] -> []
        [[value]] -> [(env, value, input)])

readArrayVariable :: String -> Int -> Parser Int
readArrayVariable name j = P (\env input -> case searchArrayVariable env name j of
        [[]] -> []
        [[value]] -> [(env, value, input)])

readMatrixVariable :: String -> Int -> Int -> Parser Int
readMatrixVariable name j k = P (\env input -> case searchMatrixVariable env name j k of
        [[]] -> []
        [[value]] -> [(env, value, input)])

-- Search the value of a variable given the name (and indeces in case of array or matrix)
-- if the env list is empty there is no variable, if it isn't empty check the head and if is isn't the searched variable check ricursively the tail
searchVariable :: Env -> String -> [[Int]]
searchVariable [] queryname = []
searchVariable (x:xs) queryname = if (name x) == queryname then [[((value x) !! 0) !! 0]]   --takes: [[int]], [int], int
else searchVariable xs queryname

searchArrayVariable :: Env -> String -> Int -> [[Int]]
searchArrayVariable [] queryname j = [[]]
searchArrayVariable (x:xs) queryname j = if ((name x) == queryname) then [[((value x) !! 0) !! j]]  --takes the element j of the only list in [[int]]
else searchArrayVariable xs queryname j

searchMatrixVariable :: Env -> String -> Int -> Int -> [[Int]]
searchMatrixVariable [] queryname j k = [[]]
searchMatrixVariable (x:xs) queryname j k = if ((name x) == queryname) then [[((value x) !! j) !! k]]    --takes the element at row j and column k (takes list j in [[int]] and then its element k)
else searchMatrixVariable xs queryname j k


-- Update the environment with a variable
updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of
        xs -> [(modifyEnv env var,"",xs)])

-- If the variable is new, it is added to the environment, if the variable already exist, its value will be overwritten
modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if name x == name newVar then newVar : xs else x : modifyEnv xs newVar


--parser for arithmetic expressions, also evaluate them

-- aexp := <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
aexp :: Parser Int
aexp = do {
        do {
            t <- aterm;      --store the output of the 'aterm' parser in t
            symbol "+";      --parse the +
            a <- aexp;       --recursively parse other aexp
            return (t + a);  --executes the operation specified in the parsed instructions
        }
        Main.<|>
        do {
			t <- aterm;
			symbol "-";
			a <- aexp;
			return (t - a);
        }
        Main.<|> aterm
      }

-- aterm := <afactor> * <aterm> | <afactor> / <aterm> | <afactor>
aterm :: Parser Int
aterm = do {
			do {
				f <- afactor;
				symbol "*";
				t <- aterm;
				return (f * t);
			}
          Main.<|>
			do{
				f <- afactor;
				symbol "/";
				t <- aterm;
				return (f `div` t);
			}
          Main.<|> afactor
        }

-- afactor := (<aexp>) | <identifier>[<aexp>][<aexp>] | <identifier>[<aexp>] | <identifier> | <integer>
afactor :: Parser Int
afactor = do {
				do{
					symbol "(";
					a <- aexp;
					symbol ")";
					return a
				}
				Main.<|>
				do{
					i <- identifier;
					symbol "[";
					j <- aexp;
					symbol "]";
					symbol "[";
					k <- aexp;
					symbol "]";
					readMatrixVariable i j k;      --read i[j][k]
				}
				Main.<|>
				do{
					i <- identifier;
					symbol "[";
					j <- aexp;
					symbol "]";
					readArrayVariable i j;
				}
				Main.<|>
				do{
					i <- identifier;
					readVariable i;
				}
				Main.<|> integer
            }

-- parse Arithmetic Expressions without evaluating them
consumeAexp :: Parser String
consumeAexp = do {
					do {
						t <- consumeAterm;
						symbol "+";
						a <- consumeAexp;
						return (t ++ "+" ++ a);
					}
					Main.<|>
					do {
						t <- consumeAterm;
						symbol "-";
						a <- consumeAexp;
						return (t ++ "-" ++ a);
						}
					Main.<|> consumeAterm
                }

consumeAterm :: Parser String
consumeAterm = do {
					do {
						f <- consumeAfactor;
						symbol "*";
						t <- consumeAterm;
						return (f ++ "*" ++ t);
						}
					Main.<|>
					do{
						f <- consumeAfactor;
						symbol "/";
						t <- consumeAterm;
						return (f ++ "/" ++ t);
					}
					Main.<|> consumeAfactor
				}

consumeAfactor :: Parser String
consumeAfactor = do {
						do{
							symbol "(";
							a <- consumeAexp;
							symbol ")";
							return ("(" ++ a ++ ")");
						}
						Main.<|>
						do{
							i <- identifier;
							symbol "[";
							j <- consumeAexp;
							symbol "]";
							symbol "[";
							k <- consumeAexp;
							symbol "]";
							return (i ++ "[" ++ j ++ "]" ++ "[" ++ k ++ "]")
						}
						Main.<|>
						do{
							i <- identifier;
							symbol "[";
							j <- consumeAexp;
							symbol "]";
							return (i ++ "[" ++ j ++ "]");
						}
						Main.<|> do { identifier }           --only parse the identifier
						Main.<|> do { show <$> integer }    --apply show to the wrapped int, trasforming it in a string
       				}



--parser for boolean expressions, also evaluate them

--bexp := <bterm> OR <bexp> | <bterm>
bexp :: Parser Bool
bexp = do{
			do{
				b0 <- bterm;
				symbol "OR";
				b1 <- bexp;
				return (b0 || b1);
			}
			Main.<|> bterm        
 		}

-- bterm := <bfactor> AND <bterm> | <bfactor>
bterm :: Parser Bool
bterm = do{
			do{
				f0 <- bfactor;
				symbol "AND";
				f1 <- bterm;
				return (f0 && f1);
			}
			Main.<|> bfactor
		}

-- bfactor := true | false | !<bfactor> | (bexp) | <bcomparison>
bfactor:: Parser Bool
bfactor = do{
				do{
					symbol "true";
					return True;
				}
			    Main.<|>
				do {
					symbol "false";
					return False;
					}
				Main.<|>
				do{
					symbol "!";
					not <$> bfactor;     --apply the not function to the bool wrapped in bfactor
				}
			    Main.<|>
				do{
					symbol "(";
					b <- bexp;
					symbol ")";
					return b;
				}
			 	Main.<|> bcomparison				
		    }

-- bcomparison := <aexp> == <aexp> | <aexp> < <aexp> | <aexp> <= <aexp> | <aexp> > <aexp> | <aexp> >= <aexp>
bcomparison:: Parser Bool
bcomparison = do {
					do{
						a0 <- aexp;
						symbol "==";
						a1 <- aexp;
						return ( a0 == a1);
					}
					Main.<|>
					do{
						a0 <- aexp;
						symbol "<";
						a1 <- aexp;
						return ( a0 < a1);
					}
					Main.<|>
					do{
						a0 <- aexp;
						symbol "<=";
						a1 <- aexp;
						return ( a0 <= a1);
					}
					Main.<|>
					do{
						a0 <- aexp;
						symbol ">";
						a1 <- aexp;
						return ( a0 > a1);
					}
					Main.<|>
					do{
						a0 <- aexp;
						symbol ">=";
						a1 <- aexp;
						return ( a0 >= a1);
					}
				}

--parse Boolean Expressions without evaluating them
consumeBexp :: Parser String
consumeBexp = do{
					do{
						b0 <- consumeBterm;
						symbol "OR";
						b1 <- consumeBexp;
						return (b0 ++ " OR " ++ b1);
					}
					Main.<|> consumeBterm			
				}

consumeBterm :: Parser String
consumeBterm = do{
					do{
						f0 <- consumeBfactor;
						symbol "AND";
						f1 <- consumeBterm;
						return (f0 ++ " AND " ++ f1);
					}
					Main.<|> consumeBfactor
				}

consumeBfactor:: Parser String
consumeBfactor = do{
						do{
							symbol "true";
							return "true";
						}
						Main.<|>
						do {
							symbol "false";
							return "false";
						}
						Main.<|>
						do{
							symbol "!";
							b <- consumeBfactor;
							return ("! " ++ b);
						}
						Main.<|>
						do{
							symbol "(";
							b <- consumeBexp;
							symbol ")";
							return ("( " ++ b ++ " )");
						}
						Main.<|> consumeBcomparison
					}

consumeBcomparison:: Parser String
consumeBcomparison = do{
							do{
								a0 <- consumeAexp;
								symbol "==";
								a1 <- consumeAexp;
								return ( a0 ++ " == " ++ a1);
								}
							Main.<|>
							do {
								a0 <- consumeAexp;
								symbol "<";
								a1 <- consumeAexp;
								return ( a0 ++ " < " ++ a1);
							}
							Main.<|>
							do {
								a0 <-  consumeAexp;
								symbol "<=";
								a1 <- consumeAexp;
								return ( a0 ++ " <= " ++ a1);
							}
							Main.<|>
							do{
								a0 <- consumeAexp;
								symbol ">";
								a1 <- consumeAexp;
								return ( a0 ++ " > " ++ a1);
							}
							Main.<|>
							do {
								a0 <- consumeAexp;
								symbol ">=";
								a1 <- consumeAexp;
								return ( a0 ++ " >= " ++ a1);
							}
						}

--Commands parsing and evaluation

-- program := <command> <program> | <command>
program :: Parser String
program = do {
				do {
					command;
					program;
				}
				Main.<|> command
			}

-- command := <assignment> | <arrayAssignment> | <matrixAssignment> | <ifThenElse> | <while> | <for> | ‘skip’ ‘;’
command :: Parser String
command = do{
				do assignment;
				Main.<|>
				do arrayAssignment;
				Main.<|>
				do matrixAssignment;
				Main.<|>
				do ifThenElse;
				Main.<|>
				do while;
				Main.<|>
				do for;
				Main.<|>
				do{
				   symbol "skip";
				   symbol ";"
				}
			}

-- assignment := <identifier> '=' <aexp>; 
assignment :: Parser String
assignment = do{
				x <- identifier;
				symbol "=";
				v <- aexp;
				symbol ";";
				updateEnv Variable{name=x,vtype="Integer", value= [[v]]};
			}

-- <arrayAssignment> := <identifier> '=' <tokenArray> ';'
arrayAssignment :: Parser String
arrayAssignment = do{
						x <- identifier;
						symbol "=";
						v <- tokenArray;
						symbol ";";
						updateEnv Variable{name=x, vtype="Array", value= [v]};   -- v=[int]
					}

-- <matrixAssignment> := <identifier> '=' <tokenMatrix> ';'
matrixAssignment :: Parser String
matrixAssignment = do{
						x <- identifier;
						symbol "=";
						v <- tokenMatrix;
						symbol ";";
						updateEnv Variable{name=x, vtype="Matrix", value= v};   -- v=[[int]]
					}

-- ifThenElse := if (<bexp>) { <program> } | if (<bexp>) {<program>} else {<program>}
ifThenElse :: Parser String
ifThenElse = do {    
				symbol "if";
				b <- bexp;
				symbol "{";
				if b then do{               --parse and evaluate the program in the 'if' branch and only parse(consume) the program in the 'else' branch
								program;
								symbol "}";
								do{
									symbol "else";
									symbol "{";
									consumeProgram;
									symbol "}";
									return "";
								}
								Main.<|> return ""    --in case there is no 'else' branch
							}
				else do{                     --only parse(consume) the program in the 'if' branch, parse and evaluate the program in the 'else' branch(if it is present)
							consumeProgram;
							symbol "}";
							do{
								symbol "else";
								symbol "{";
								program;
								symbol "}";
								return "";
							}
							Main.<|> return ""			--in case there is no 'else' branch		
						}
			}

-- while := while (<bexp>) {<program>}
while :: Parser String
while = do {
			w <- consumeWhile;
			repeatBlock w;
			symbol "while";
			b <- bexp;
			symbol "{";
			if b then do{
						program;      --parse and evaluate the 'while' body
						symbol "}";
						repeatBlock w;
						while;
						} 
			else do {
					consumeProgram;    -- only parse the 'while' body
					symbol "}";
					return "";
					}
		}


repeatBlock :: String -> Parser String
repeatBlock c = P(\env input -> [(env, "", c ++ input)])

for :: Parser String
for = do{
		w <- consumeFor;
		repeatBlock w;
		symbol "for";
		symbol "(";
		assignment;       --already terminates with ';'
		b <- bexp;
		symbol ";";
		a <- consumeAssignment;
		symbol ")";
		symbol "{";
		if b then do{
					program;
					repeatBlock a;
					assignment;
					symbol "}";
					repeatBlock w;
					forLoop;         --doesn't execute the first assignment, only parse it
				  } 
		else do {
				consumeProgram;
				symbol "}";
				return "";
				}
	}


forLoop:: Parser String
forLoop = do{
				w <- consumeFor;
				repeatBlock w;
				symbol "for";
				symbol "(";
				consumeAssignment;
				b <- bexp;
				symbol ";";
				a <- consumeAssignment;
				symbol ")";
				symbol "{";
				if b then do{
							program;
							repeatBlock a;
							assignment;
							symbol "}";
							repeatBlock w;
							forLoop;
				        	} 
				else do{
						consumeProgram;
						symbol "}";
						return "";
						}
			}


consumeProgram :: Parser String
consumeProgram = do{
					do{ 
						c <- consumeCommand;
						p <- consumeProgram;
						return (c ++ p);
					}
					Main.<|> consumeCommand
				  }

consumeCommand :: Parser String
consumeCommand = do{
					do{consumeAssignment;}
					Main.<|>
					do{consumeArrayAssignment;}
					Main.<|>
					do{consumeMatrixAssignment;}
					Main.<|>
					do{consumeifThenElse;}
					Main.<|>
					do{consumeWhile;}
					Main.<|>
					do{consumeFor;}
					Main.<|>
					do{
						symbol "skip";
						symbol ";";
						return "skip;"
					}
				}

consumeAssignment :: Parser String
consumeAssignment = do{
						x <- identifier;
						symbol "=";
						a <- consumeAexp;
						symbol ";";
						return (x ++ "=" ++ a ++ ";");
					  }

consumeArrayAssignment :: Parser String
consumeArrayAssignment = do{
							x <- identifier;
							symbol "=";
							v <- consumeArray;
							symbol ";";
							return (x ++ "=" ++ v ++ ";")
							}	

consumeArray :: Parser String
consumeArray = do {
					char '[';
					space;
					a <- consumeArrayContent;
					space;
					char ']';
					return ("[ " ++ a ++ " ]");
         		}

consumeArrayContent :: Parser String
consumeArrayContent = do{
							a0 <- consumeAexp;
							space;
							char ',';
							space;
							a1 <- consumeArrayContent;
							return (a0 ++ " , " ++ a1);
						}
						Main.<|> 
						do{                 --if the array has only one element
							space;
							a0 <- consumeAexp;
							space;
							return a0;
						}


consumeMatrixAssignment :: Parser String
consumeMatrixAssignment = do{
								x <- identifier;
								symbol "=";
								v <- consumeMatrix;
								symbol ";";
								return (x ++ "=" ++ v ++ ";")
							}


consumeMatrix :: Parser String
consumeMatrix = do {
					char '[';
					space;
					a <- consumeMatrixContent;
					space;
					char ']';
					return ("[ " ++ a ++ " ]");
					}


consumeMatrixContent :: Parser String
consumeMatrixContent = do{
							space;
							a0 <- consumeArray;
							space;
							char ',';
							a1 <- consumeMatrixContent;
							return (a0 ++ " , " ++ a1);
						}
				Main.<|> 
						do{
							space;
							a <- consumeArray;
							space;
							return a;
						}


consumeifThenElse :: Parser String
consumeifThenElse = do{
						symbol "if";
						b <- consumeBexp;
						symbol "{";
						p0 <- consumeProgram;
						symbol "}";
						do{
							symbol "else";
							symbol "{";
							p1 <- consumeProgram;
							symbol "}";
							return (" if " ++ b ++ " { " ++ p0 ++ " } " ++ " else { " ++ p1 ++ " } ");
						}
						Main.<|> return (" if " ++ b ++ " { " ++ p0 ++ " } ")
					}


consumeWhile :: Parser String
consumeWhile = do{
					symbol "while";
					b <- consumeBexp;
					symbol "{";
					p <- consumeProgram;
					symbol "}";
					return("while " ++ b ++ " {" ++ p ++ "}");
				}

consumeFor :: Parser String
consumeFor = do{
					symbol "for";
					symbol "(";
					a0 <- consumeAssignment;     --already has ';'
					b <- consumeBexp;
					symbol ";";
					a1 <- consumeAssignment;
					symbol ")";
					symbol "{";
					p <- consumeProgram;
					symbol "}";
					return("for ( " ++ a0 ++ b ++ " ; " ++ a1 ++ ") {" ++ p ++ "}");
				}

