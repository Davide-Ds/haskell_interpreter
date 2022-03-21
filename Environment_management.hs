module Environment_management  where
import Core


-- Return the value of a variable given the name (and given the indeces in case of array or matrix)
readVariable :: String -> Parser Numeric
readVariable name = P (\env input -> case searchVariable env name of
        [[]] -> []
        [[value]] -> [(env, value, input)])

readWholeArray :: String -> Parser [Numeric]
readWholeArray name = P (\env input -> case searchVariable env name of
        [[]] -> []
        [x:xs] -> [(env, x:xs, input)])		

-- read single array elements eg.x[i]
readArrayVariable :: String -> Int -> Parser Numeric
readArrayVariable name j = P (\env input -> case searchArrayVariable env name j of
        [[]] -> []
        [[value]] -> [(env, value, input)])

-- read single matrix elements eg.x[i][j]
readMatrixVariable :: String -> Int -> Int -> Parser Numeric
readMatrixVariable name j k = P (\env input -> case searchMatrixVariable env name j k of
        [[]] -> []
        [[value]] -> [(env, value, input)])

-- Search the value of a variable given the name (and indeces in case of array or matrix)
-- if the env list is empty there is no variable, if it isn't empty check the head and if is isn't the searched variable check ricursively the tail
searchVariable :: Env -> String -> [[Numeric]]
searchVariable [] queryname = []
searchVariable (x:xs) queryname|(name x == queryname) && (vtype x == "Array") = [(value x) !! 0]           --get: [[Numeric]], [Numeric]
                               |name x == queryname = [[((value x) !! 0) !! 0]]                            --get: [[Numeric]], [Numeric], Numeric 
							   |otherwise = searchVariable xs queryname

-- search single array elements eg.x[i]
searchArrayVariable :: Env -> String -> Int -> [[Numeric]]
searchArrayVariable [] queryname j = [[]]
searchArrayVariable (x:xs) queryname j = if ((name x) == queryname) then [[((value x) !! 0) !! j]]   --takes the element j of the only list in [[int]]
else searchArrayVariable xs queryname j

-- replace an element in an array with a new one
replace :: Int -> a -> [a] -> [a]
replace pos newVal list | length list >= pos = take pos list ++ newVal : drop (pos+1) list
                        | otherwise = list 

searchMatrixVariable :: Env -> String -> Int -> Int -> [[Numeric]]
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

--print the environment content without type information
showMemoryState :: Env -> String
showMemoryState []             = []
showMemoryState (x:xs) | vtype x == "Numeric" = name x ++ "=" ++ numericToString (value x !! 0 !! 0) ++ " " ++ showMemoryState xs         --(value x !! 0 !! 0) take the first element of the first list in [[Numeric]]
                       | vtype x == "Array" = name x ++ "=" ++ "[" ++ arrayNumericToString (value x !! 0) ++ " " ++ showMemoryState xs    --(value x !! 0) take the first list in [[Numeric]]
					   | otherwise = name x ++ "=" ++ "[" ++ matrixNumericToString (value x) ++ "] " ++ " " ++ showMemoryState xs         --vtype is matrix so (value x) take all [[Numeric]]

--print raw environment content
getMemory :: [(Env, String, String)] -> String
getMemory [] = "Invalid input\n"
getMemory [(x:xs, parsedString, "")] = vtype x ++ "  " ++ name x ++ " = " ++ show (value x) ++ "\n" ++ getMemory [(xs,parsedString,"")]
getMemory [(env, parsedString, unparsedString)] = case unparsedString of
    "" -> ""
    _  -> "Error (unused input '" ++ unparsedString ++ "')\n" ++ getMemory [(env,parsedString, "")]
