{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
import Environment_management ( showMemoryState, getMemory ) 
import Core ( Env, parse )  
import Parse_and_execute ( program ) 
import Only_parse 
import System.IO (hFlush, stdout)

-- takes the first element of a triple
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

--take in input the parsing and execution output (memory state, parsed input, unparsed input)
interpreter :: [(Env, String, String)] -> ([Env],String)
-- case: empty list
interpreter [] = ([],"[ERROR] Invalid input!\n")

-- case: entire input string consumed, the parsing was ok
interpreter [(env, parsedString, "")] = ([newEnv],
        "\nExecution result: " ++ showMemoryState newEnv ++
    "\n\nMemory: \n" ++ getMemory parsed)
    where
        parsed = [(env, parsedString, "")]                
        newEnv = if not (null parsed) then fst3 (head parsed)
                 else env

-- case: input string not entirely consumed
interpreter [(env, parsedString, unparsedString)] = ([env],
        "\nExecution result: " ++ "error" ++
    "\n\nMemory: \n" ++ getMemory (parse program [] parsedString) ++
    "\nError: \nUnused input '" ++ unparsedString ++ "'\n")

-- parse the program and call the interpreter
menu :: [Env] -> IO String
menu [] = menu [[]]           --create empty env
menu [env] = do {putStr "====================== \nPImp> ";
             hFlush stdout;
             input <- getLine;
             if input == "-q" then return "Bye!";
             else do let result = interpreter (parse program env input)       --call the 'program' parser which parse and executes
                     putStrLn (snd result)
                     menu [[]]            
            }

-- Interpreter Interface
logo :: IO String
logo = do putStrLn "\n ██████╗   ██╗                   "
          putStrLn " ██   ██╗  ██║ ███╗   ███╗ ██████╗ "
          putStrLn " ██╔══██║  ██║ ████╗ ████║ ██╔══██╗"
          putStrLn " ██████╔╝  ██║ ██╔████╔██║ ██████╔╝"
          putStrLn " ██╔═══╝   ██║ ██║╚██╔╝██║ ██╔═══╝ "
          putStrLn " ██║       ██║ ██║ ╚═╝ ██║ ██║     "
          putStrLn " Parser and Interpreter for an imperative language "
          putStrLn "            by Davide De Simone\n"
          putStrLn "Enter the code to be evaluated (ending with ;)\nor type '-q' to quit.\n"
          menu [];

--start the interpreter 
main :: IO String
main = logo;
