{-# OPTIONS_GHC -fno-warn-tabs #-}
module Parse_and_execute where
import Core
import Environment_management
import Only_parse
   


--parser for arithmetic expressions, also evaluate them

-- aexp := <aterm> + <aexp> | <aterm> - <aexp> | <aterm>
aexp :: Parser Numeric
aexp = do {
        do {
            t <- aterm;      --store the output of the 'aterm' parser in t
            symbol "+";      --parse the +
            a <- aexp;       --recursively parse other aexp
            return (F (numericToFloat t + numericToFloat a))    --executes the operation specified in the parsed instructions
        }
        <|>
        do {
			t <- aterm;
			symbol "-";
			a <- aexp;
			return (F (numericToFloat t - numericToFloat a))
        }
        <|> aterm
      }

-- aterm := <afactor> * <aterm> | <afactor> / <aterm> | <afactor> ^ <aterm> | <afactor> % <aterm> |<afactor>
aterm :: Parser Numeric
aterm = do {
			do {
				f <- afactor;
				symbol "*";
				t <- aterm;
				return (F (numericToFloat f * numericToFloat t))
			}
        <|>
			do{
				f <- afactor;
				symbol "/";
				t <- aterm;
				return (F (numericToFloat f / numericToFloat t))
			}
		<|>
			do{
				f <- afactor;
				symbol "^";
				t <- aterm;
				if checkInt (numericToFloat f) then 
				   return (I (numericToInt f ^ numericToInt t)) 
				else 
				   return (F (numericToFloat f ** numericToFloat t))				
			}
		<|>
			do{
				f <- afactor;
				symbol "%";
				t <- aterm;
				return (I (numericToInt f  `mod` numericToInt t))
			}
        <|> afactor
        }

-- afactor := (<aexp>) | <identifier>[<aexp>][<aexp>] | <identifier>[<aexp>] | <identifier> | <integer> | <float>
afactor :: Parser Numeric
afactor = do {
				do{
					symbol "(";
					a <- aexp;
					symbol ")";
					return a
				}
				<|>
				do{
					i <- identifier;
					symbol "[";
					j <- aexp;
					symbol "]";
					symbol "[";
					k <- aexp;
					symbol "]";
					readMatrixVariable i (numericToInt j) (numericToInt k);      --read i[j][k]
				}
				<|>
				do{
					i <- identifier;
					symbol "[";
					j <- aexp;
					symbol "]";
					readArrayVariable i (numericToInt j);
				}
				<|>
				do{
					i <- identifier;
					readVariable i;
				}				 
				<|> fmap F float         --first float parser then int to avoid error when parsing floats
				<|> fmap I integer
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
			<|> bterm        
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
			<|> bfactor
		}

-- bfactor := true | false | !<bfactor> | (bexp) | <bcomparison>
bfactor:: Parser Bool
bfactor = do{
				do{
					symbol "true";
					return True;
				}
			    <|>
				do {
					symbol "false";
					return False;
					}
				<|>
				do{
					symbol "!";
					fmap not bfactor;     --apply the not function to the bool wrapped in bfactor
				}
			    <|>
				do{
					symbol "(";
					b <- bexp;
					symbol ")";
					return b;
				}
			 	<|> bcomparison				
		    }

-- bcomparison := <aexp> == <aexp> | <aexp> != <aexp> |<aexp> < <aexp> | <aexp> <= <aexp> | <aexp> > <aexp> | <aexp> >= <aexp>
bcomparison:: Parser Bool
bcomparison = do {
					do{
						a0 <- aexp;
						symbol "==";
						a1 <- aexp;
						return ( numericToInt a0 == numericToInt a1);
					}
					<|>
					do{
						a0 <- aexp;
						symbol "!=";
						a1 <- aexp;
						return ( numericToInt a0 /= numericToInt a1);
					}
					<|>
					do{
						a0 <- aexp;
						symbol "<";
						a1 <- aexp;
						return ( numericToInt a0 < numericToInt a1);
					}
					<|>
					do{
						a0 <- aexp;
						symbol "<=";
						a1 <- aexp;
						return (numericToInt a0 <= numericToInt a1);
					}
					<|>
					do{
						a0 <- aexp;
						symbol ">";
						a1 <- aexp;
						return ( numericToInt a0 > numericToInt a1);
					}
					<|>
					do{
						a0 <- aexp;
						symbol ">=";
						a1 <- aexp;
						return ( numericToInt a0 >= numericToInt a1);
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
				<|> command
			}

-- command := <assignment> | <arrayAssignment> | <arrayValueAssignment> | <matrixAssignment> | <ifThenElse> | <switch> | <while> | <for> | ‘skip’ ‘;’
command :: Parser String
command = do{
				do assignment;
				<|>
				do arrayAssignment;
				<|>
				do arrayValueAssignment;
				<|>
				do matrixAssignment;
				<|>
				do ifThenElse;
				<|>
				do switch;
				<|>
				do while;
				<|>
				do for;
				<|>
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
				updateEnv Variable{name=x,vtype="Numeric", value= [[v]]};
			}

-- <arrayAssignment> := <identifier> '=' <tokenArray> ';'
arrayAssignment :: Parser String
arrayAssignment = do{
						x <- identifier;
						symbol "=";
						v <- tokenArray;
						symbol ";";
						updateEnv Variable{name=x, vtype="Array", value= [v]};   -- v=[Numeric]
					}


-- <arrayValueAssignment> := <identifier>[i] '=' <aexp> ';' 
arrayValueAssignment :: Parser String
arrayValueAssignment = do{
							x <- identifier;
							symbol "[";
							i <- aexp;
							symbol "]";
							symbol "=";
							v <- aexp;
							symbol ";";
							arr <- readWholeArray x;
							updateEnv Variable{name=x, vtype="Array", value= [replace (numericToInt i) v arr]};     --replace the old array with the one containing the new value
						}

-- <matrixAssignment> := <identifier> '=' <tokenMatrix> ';'
matrixAssignment :: Parser String
matrixAssignment = do{
						x <- identifier;
						symbol "=";
						v <- tokenMatrix;
						symbol ";";
						updateEnv Variable{name=x, vtype="Matrix", value= v};   -- v=[[Numeric]]
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
								<|> return ""    --in case there is no 'else' branch
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
							<|> return ""			--in case there is no 'else' branch		
						}
			}

-- <switch>:= switch ( aexp ) { <case_stmt> } 
switch :: Parser String
switch = do { 
	        symbol "switch";
			a <- aexp;          -- aexp already parse the ( )
			symbol "{";
			case_stmt a;    -- the Numeric 'a' will be compared with the 'case' condition
			symbol "}";
			}

-- <case_stmt> :=  case <integer> : <program> <case_stmt> | default : <program>  
case_stmt :: Numeric -> Parser String
case_stmt  a = do { 
				symbol "case";
				i <- integer;
				symbol ":";
				if i == numericToInt a then          --if the 'case' condition match the aexp then its code is executed and the rest of the switch only parsed
				 		do { 
				 			program;
				 			consumeCaseStmt;
				 			}
				else do { 				 			--if the 'case' condition is not matched by the aexp then its code is parsed and are tried the other case
						consumeProgram;
						case_stmt a;
						}		
				}
			<|>
			do {                        -- if no case condition is matched then is execuded the default branch
				symbol "default:";
				program;
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

-- <for> := <consumeFor> for(<assignment> <bexp> ; <consumeAssignment>) { <program> <assignment> } <forLoop> | <consumeFor> for (<assignment> <bexp> ; <consumeAssignment>) { }
for :: Parser String
for = do{
		w <- consumeFor;
		repeatBlock w;
		symbol "for";
		symbol "(";
		assignment;       --already parse ';'
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

-- <forLoop> := <consumeFor> for(<consumeAssignment> <bexp> ; <consumeAssignment> ) {<program> <assignment>} <forLoop> | <consumeFor> for( <consumeAssignment> <bexp> ; <consumeAssignment> ) {<parseProgram>}
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

-- <array> := [ <arrayContent> ]
--parse the array brackets and (eventual)spaces after them
array :: Parser [Numeric]
array = do {
			char '[';                   --parse the [
			space;                      --parse 0 o more spaces
			a <- arrayContent;          --parse the array content
			space;
			char ']';
			return a;
        }

-- <arrayContent> := <aexp> , <arrayContent> | <aexp>
--parse the array elements, spaces and commas after them
arrayContent :: Parser [Numeric]
arrayContent = do {
                a0 <- aexp;       --parse the first element of the array
                space;
                char ',';
                a1 <- arrayContent;    --parse the recursively the other elements
                return (a0 : a1);      -- return the parsed array content
        }
        <|> do{               --in case of an array with only one element
                space;
                a0 <- aexp;
                space;
                return [a0];
        }

-- <matrix> := [ <matrixContent> ]
--parse the matrix external brackets and (eventual)spaces after them
matrix :: Parser [[Numeric]]
matrix = do {
                char '[';
                space;
                a <- matrixContent;
                space;
                char ']';
                return a;
        }

-- <matrixContent> := <array> , <matrixContent> | <array>
--parse all the arrays which composes the matrix
matrixContent :: Parser [[Numeric]]
matrixContent = do {
                space;
                a0 <- array;      --parse the first list(row) of the 'list of list'(matrix)
                space;
                char ',';
                a1 <- matrixContent;     --parse the other rows, ricursively
                return (a0 : a1);
        }
        <|> do{                    --the matrix has only one row
                space;
                a <- array;
                space;
                return [a];
        }


--to parse 0 or more spaces

tokenArray :: Parser [Numeric]
tokenArray = token array

tokenMatrix :: Parser [[Numeric]]
tokenMatrix = token matrix