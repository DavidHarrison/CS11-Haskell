-- File: hcat.hs
-- concatenate given files xor stdin

-- provides hGetContents
import System.IO
-- provides support for choosing the right files to access (arguments/stdin)
import Files

main :: IO ()
main = all_text >>= putStr
  where
    -- the text from all files
    all_text :: IO String
    all_text = file_texts >>= return . concat
    -- a list of text for each file
    file_texts :: IO [String]
    file_texts = files >>= mapM (hGetContents)
    -- hGetContents :: Handle -> IO String
    -- files :: IO [Handle]
