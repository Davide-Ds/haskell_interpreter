module Only_parse where
import Core


-- parse Arithmetic Expressions without evaluating them
consumeAexp :: Parser String
consumeAexp = do {
					do {
						t <- consumeAterm;
						symbol "+";
						a <- consumeAexp;
						return (t ++ "+" ++ a);
					}
					<|>
					do {
						t <- consumeAterm;
						symbol "-";
						a <- consumeAexp;
						return (t ++ "-" ++ a);
						}
					<|> consumeAterm
                }

consumeAterm :: Parser String
consumeAterm = do {
					do {
						f <- consumeAfactor;
						symbol "*";
						t <- consumeAterm;
						return (f ++ "*" ++ t);
						}
					<|>
					do{
						f <- consumeAfactor;
						symbol "/";
						t <- consumeAterm;
						return (f ++ "/" ++ t);
					}
					<|>
					do{
						f <- consumeAfactor;
						symbol "^";
						t <- consumeAterm;
						return (f ++ "^" ++ t);
					}
					<|>
					do{
						f <- consumeAfactor;
						symbol "%";
						t <- consumeAterm;
						return (f ++ "%" ++ t);
					}
					<|> consumeAfactor
				}

consumeAfactor :: Parser String
consumeAfactor = do {
						do{
							symbol "(";
							a <- consumeAexp;
							symbol ")";
							return ("(" ++ a ++ ")");
						}
						<|>
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
						<|>
						do{
							i <- identifier;
							symbol "[";
							j <- consumeAexp;
							symbol "]";
							return (i ++ "[" ++ j ++ "]");
						}
						<|> do identifier          --only parse the identifier
						<|> fmap show float       --first float parser then int to avoid error when parsing floats 
						<|> fmap show integer    --apply show to the wrapped int, trasforming it in a string
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
					<|> consumeBterm			
				}

consumeBterm :: Parser String
consumeBterm = do{
					do{
						f0 <- consumeBfactor;
						symbol "AND";
						f1 <- consumeBterm;
						return (f0 ++ " AND " ++ f1);
					}
					<|> consumeBfactor
				}

consumeBfactor:: Parser String
consumeBfactor = do{
						do{
							symbol "true";
							return "true";
						}
						<|>
						do {
							symbol "false";
							return "false";
						}
						<|>
						do{
							symbol "!";
							b <- consumeBfactor;
							return ("! " ++ b);
						}
						<|>
						do{
							symbol "(";
							b <- consumeBexp;
							symbol ")";
							return ("( " ++ b ++ " )");
						}
						<|> consumeBcomparison
					}

consumeBcomparison:: Parser String
consumeBcomparison = do{
							do{
								a0 <- consumeAexp;
								symbol "==";
								a1 <- consumeAexp;
								return ( a0 ++ " == " ++ a1);
								}
							<|>
							do {
								a0 <- consumeAexp;
								symbol "<";
								a1 <- consumeAexp;
								return ( a0 ++ " < " ++ a1);
							}
							<|>
							do {
								a0 <-  consumeAexp;
								symbol "<=";
								a1 <- consumeAexp;
								return ( a0 ++ " <= " ++ a1);
							}
							<|>
							do{
								a0 <- consumeAexp;
								symbol ">";
								a1 <- consumeAexp;
								return ( a0 ++ " > " ++ a1);
							}
							<|>
							do {
								a0 <- consumeAexp;
								symbol ">=";
								a1 <- consumeAexp;
								return ( a0 ++ " >= " ++ a1);
							}
						}



consumeProgram :: Parser String
consumeProgram = do{
					do{ 
						c <- consumeCommand;
						p <- consumeProgram;
						return (c ++ p);
					}
					<|> consumeCommand
				  }

consumeCommand :: Parser String
consumeCommand = do{
					do{consumeAssignment;}
					<|>
				    do consumeArrayValueAssignment;
					<|>
					do{consumeArrayAssignment;}
					<|>
					do{consumeMatrixAssignment;}
					<|>
					do{consumeifThenElse;}
					<|>
					do{consumeSwitch;}
					<|>
					do{consumeWhile;}
					<|>
					do{consumeFor;}
					<|>
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

consumeArrayValueAssignment :: Parser String
consumeArrayValueAssignment = do{
								x <- identifier;
								symbol "[";
								i <- consumeAexp;
								symbol "]";
								symbol "=";
								v <- consumeAexp;
								symbol ";";
								return (x ++ "[" ++ i ++ "]" ++ "=" ++ v ++ ";")
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
						<|> 
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
				<|> 
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
						<|> return (" if " ++ b ++ " { " ++ p0 ++ " } ")
					}

consumeSwitch :: Parser String
consumeSwitch = do { 
					symbol "switch";
					a <- consumeAexp;
					symbol "{";
					c <- consumeCase_stmt;
					symbol "}";
					return ("switch" ++ a ++ "{" ++ c ++ "}");
				   }

-- <case_stmt> :=  ‘case’ <integer> ‘:’ <program> <case_stmt> | ‘default’ ‘:’ <program>  
consumeCase_stmt :: Parser String
consumeCase_stmt = do { 
						symbol "case";
						i <- integer;
						symbol ":";
						p <- consumeProgram;				
						c <- consumeCase_stmt;
						return ("case" ++ show i ++ ":" ++ p ++ c );		
						}
					<|>
					do {
						symbol "default";
						symbol ":";
						consumeProgram;
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
