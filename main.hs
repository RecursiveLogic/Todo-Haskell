putTodo :: (Int, String) -> IO ()
putTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
    putStrLn "\nCurrent To-do List:"
    mapM_ putTodo (zip [0..] todos)
    command <- getLine
    getCommand command todos

getCommand :: String -> [String] -> IO ()
getCommand ('+':' ':todo) todos = prompt (todo:todos)
getCommand ('-':' ':num) todos = 
    case delete (read num) todos of
        Nothing -> do
            putStrLn "No To-do entry matches the given number"
            prompt todos
        Just todos' -> prompt todos'
getCommand "Q"      todos = return ()
getCommand command  todos = do
    putStrLn ("Invalid Command: '" ++ command ++ "'")
    prompt todos

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    let x = n - 1
    y <- x `seq` delete x as
    return (a:y)
delete _ [] = Nothing

main = do
    putStrLn "\nCommands:"
    putStrLn "+ <String>  - Add a To-do entry"
    putStrLn "- <Integer> - Delete the entry number"
    putStrLn "Q           - Quit"
    prompt []