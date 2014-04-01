-- file: huniq.hs
-- remove all consecutive non-unique lines

-- provies hGetContents
import System.IO
-- provides files (a list of files of which the first is either
--   the first file argument of if none, stdin)
import Files

main :: IO ()
main = file >>= hGetContents >>= putStrLn . unlines . rmNonUnique . lines
  where
    file :: IO Handle
    file = files >>= return . head
    -- files :: IO [Handle]

-- recursively remove all consecutive duplicated lines
-- (cannot use elem, becuause it would remove all duplicates)
rmNonUnique :: [String] -> [String]
-- not used unless stdin in empty
rmNonUnique [] = []
-- base case: only one element in array
--   (when calling head would result in a error)
rmNonUnique [x] = [x]
-- recursively build up a list of unique elements of the list,
--   only adding a line if it is not equal to the next
rmNonUnique (x:xs)
  -- if x is the same as the first element of the rest of the list,
  --   discard it
  | x == (head xs) = rmNonUnique xs
  -- otherwise, keep it (append it to the rest of the list being built)
  | otherwise = x:(rmNonUnique xs)
