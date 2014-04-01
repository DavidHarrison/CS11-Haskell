main :: IO ()
main = helloWorld2

helloWorld2 :: IO ()
helloWorld2 = putStrLn "enter name: " >>
              getLine >>= putStrLn . helloify 

helloify :: String -> String
helloify str = "hello, " ++ str ++ "!"
