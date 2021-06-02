sayHello :: String -> IO ()
sayHello x = putStrLn("Hello " ++ x ++ "!")

sayHello' :: String -> String
sayHello' x = "Hello " ++ x ++ "!"


triple = \n -> n * 3
