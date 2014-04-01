-- file: hsort.hs
-- sort lines from stdin

-- provides hGetContents
import System.IO
-- provides files (a list of files of which the first is either
--   the first file argument of if none, stdin)
import Files

main :: IO ()
main = file >>= hGetContents >>= putStrLn . unlines . quickSort . lines
  where
    file :: IO Handle
    file = files >>= return . head
    -- files :: IO [Handle]

-- use a quick sort to sort strings in the list
quickSort :: [String] -> [String]
quickSort [] = []
quickSort (x:xs) = lower ++ x:higher
  where lower = quickSort (filter (x >=) xs)
        higher = quickSort (filter (x <) xs)
