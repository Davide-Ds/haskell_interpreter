{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Environment_management ( showMemoryState, getMemory )
import Core
import Parse_and_execute ( program )
import Only_parse (consumeProgram)
import System.IO (hFlush, stdout)


-- interpreter :: Env -> String -> String
-- interpreter env xs = case (parse program env xs) of         --'program' is the program parser
--         [(env, n, [])] -> showMemoryState env
--         [(env, _, out)] -> error ("Unprocessed input string : " ++ out)
--         [] -> error "Wrong input"

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

--take in input the parsing output (memory state, parsed input, unparsed input)
interpreter :: [(Env, String, String)] -> ([Env],String)
-- case: empty list
interpreter [] = ([],"[ERROR] Invalid input!\n")

-- case: entire input string consumed, the parsing was ok
interpreter [(env, parsedString, "")] = ([newEnv],
        "\nExecution result: " ++ showMemoryState newEnv ++
    "\n\nMemory: \n" ++ getMemory parsed)
    where
        parsed = parse program env parsedString                 --call the 'program' parser which parse and executes
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
menu [env] = do {putStr "====================== \nIIMP> ";
             hFlush stdout;
             input <- getLine;
             if input == ":q" then return "Bye!";
             else do let res = interpreter (parse consumeProgram env input)
                     putStrLn (snd res)
                     menu [[]]            
            }

-- Interpreter Interface
logo :: IO String
logo = do putStrLn "\n ██████╗   ██                     "
          putStrLn " ██╔══██╗  ██╗ ███╗   ███╗██████╗ "
          putStrLn " ██╔══██╗  ██║ ████╗ ████║██╔══██╗"
          putStrLn " ██████╔╝  ██║ ██╔████╔██║██████╔╝"
          putStrLn " ██╔═══╝   ██║ ██║╚██╔╝██║██╔═══╝ "
          putStrLn " ██║       ██║ ██║ ╚═╝ ██║██║     "
          putStrLn " Parser and Interpreter for an imperative language "
          putStrLn "            by Davide De Simone\n"
          putStrLn "Enter the code to be evaluated (ending with ;)\nor type ':q' to quit.\n"
          menu [];

--start the interpreter 
main :: IO String
main = logo;
