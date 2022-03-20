
import Environment_management ( showMemoryState )
import Core ( Env, parse )
import Parse_and_execute ( program )


interpreter :: Env -> String -> String
interpreter env xs = case (parse program env xs) of         --'program' is the program parser
        [(env, n, [])] -> showMemoryState env
        [(env, _, out)] -> error ("Unprocessed input string : " ++ out)
        [] -> error "Wrong input"


