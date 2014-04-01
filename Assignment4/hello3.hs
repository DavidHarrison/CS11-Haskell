import qualified System.Environment as Env

main :: IO ()
main = helloWorld3

helloWorld3 :: IO ()
helloWorld3 = Env.getArgs >>= printStrs

printStrs :: [String] -> IO ()
printStrs [] = return ()
printStrs (s:ss) = return s >>= putStrLn
                   >> printStrs ss
