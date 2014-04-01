-- file: Files.hs
-- local functions to get a list of file from the arguments

module Files (files) where

-- provides getArgs
import System.Environment
-- provides stdin, Handle, openFile, ReadMode
import System.IO

files :: IO [Handle]
-- if arg_files is empty, then stdin will be the tail, otherwise,
--   the tail will be arg_files
files = all_files >>= return . tail'
  where
    -- tail is the last elements, including the head if a singleton
    tail' :: [a] -> [a]
    tail' (x:[]) = [x]
    tail' xs = tail xs

all_files :: IO [Handle]
all_files = arg_files >>= return . (stdin:)
-- stdin :: Handle

-- create a list of IO Handles for each argument that represents a file
arg_files :: IO [Handle]
arg_files = file_names >>= sequence . map open
  where
    open :: String -> IO Handle
    open f = openFile f ReadMode
    file_names :: IO [String]
    file_names = getArgs >>= return . filter isFile
    -- getArgs :: IO [String]

-- check whether the argument is a file
isFile :: String -> Bool
-- not used, suppresses compiler warning
isFile "" = False
-- check that the name is not an option
--   (first character is not a -)
isFile (c:_) = (c /= '-')
