--
-- FILE: triangle_game.hs
--

--
-- This program finds all solutions of the 15-peg triangle game.
--

-- import Control.Monad
import Data.Array
import qualified Data.List as List
import qualified Data.Traversable as Traversable


----------------------------------------------------------------------
-- Types.
----------------------------------------------------------------------

-- A move is three integers representing the location of the starting peg,
-- the location of the peg to jump over, and the final location.
type Move  = (Int, Int, Int)

-- The board is a list of integers, arranged in ascending order.
type Board = [Int]

-- A board solution is a starting board along with
-- a list of (move, board) pairs in the order that they were made,
-- ending in a board with only one peg.
type Solution = (Board, [(Move, Board)])


----------------------------------------------------------------------
-- Global data structures.
----------------------------------------------------------------------

all_board :: Board
all_board = [0..14]

starting_board :: Board
starting_board = [0..3] ++ [5..14]

moves :: [Move]
moves = allMoves layers
  where layers = 5 -- the final layer number for a 15 peg board (layerNum 14)

allMoves :: Int -> [Move]
allMoves layers = concat $ map (movesAt' layers) [0..last_num]
  where
    movesAt' l p = movesAt p l
    last_num = triangularNum layers

-- TODO clean up algorithm
triangularNum :: Int -> Int
triangularNum layers = round (((fromIntegral (layers * (layers + 1))) / 2) - 1)

movesAt :: Int -> Int -> [Move]
movesAt pt layers = l ++ r ++ ne ++ se ++ sw ++ nw
  where
    -- move Left
    l
      | distance_left >= move_length = [(pt, pt - 1, pt - 2)]
      | otherwise = []
    -- move Right
    r
      | distance_right >= move_length = [(pt, pt + 1, pt + 2)]
      | otherwise = []
    -- move Northeast
    ne
      | distance_right >= move_length && distance_up >= move_length =
        [(pt, pt - (layer - 1), pt - (2 * layer - 3))]
      | otherwise = []
    -- move Southeast
    se
      | distance_down >= move_length =
        [(pt, pt + (layer + 1), pt + (2 * layer + 3))]
      | otherwise = []
    -- move Southwest
    sw
      | distance_down >= move_length =
        [(pt, pt + layer, pt + (2 * layer + 1))]
      | otherwise = []
    -- move Northwest
    nw
      | distance_left >= move_length && distance_up >= move_length =
        [(pt, pt - layer, pt - (2 * layer - 1))]
      | otherwise = []
    distance_right = (triangularNum layer) - pt
    -- layer number is also base length
    distance_left = pt - (triangularNum (layer - 1) + 1)
    distance_up = layer - 1
    distance_down = layers - layer
    layer = layerNum pt
    move_length = 2

-- TODO clean up algorithm
layerNum :: Int -> Int
-- truncates to Int
layerNum pt = floor reverseTriangular
  where
    reverseTriangular = (-1.0 + root) / 2.0 + 0.9
    root = sqrt $ fromIntegral (9 + 8 * pt)

{-
moves = [( 0,  1,  3),
         ( 3,  1,  0),
         ( 1,  3,  6),
         ( 6,  3,  1),
         ( 3,  6, 10),
         (10,  6,  3),
         ( 2,  4,  7),
         ( 7,  4,  2),
         ( 4,  7, 11),
         (11,  7,  4),
         ( 5,  8, 12),
         (12,  8,  5),
         ( 0,  2,  5),
         ( 5,  2,  0),
         ( 2,  5,  9),
         ( 9,  5,  2),
         ( 5,  9, 14),
         (14,  9,  5),
         ( 1,  4,  8),
         ( 8,  4,  1),
         ( 4,  8, 13),
         (13,  8,  4),
         ( 3,  7, 12),
         (12,  7,  3),
         ( 3,  4,  5),
         ( 5,  4,  3),
         ( 6,  7,  8),
         ( 8,  7,  6),
         ( 7,  8,  9),
         ( 9,  8,  7),
         (10, 11, 12),
         (12, 11, 10),
         (11, 12, 13),
         (13, 12, 11),
         (12, 13, 14),
         (14, 13, 12)]
-}


----------------------------------------------------------------------
-- Input/output.
----------------------------------------------------------------------

-- TODO print as centered triangle

-- Pretty-print the board in a triangle shape, with '.' for unoccupied pegs
-- and the peg number for occupied pegs.
-- 11 lines.
print_board :: Board -> IO ()
print_board b = sequence (map (putStr . show_peg) all_board)
                >> putStrLn ""
  where
    show_peg :: Int -> String
    show_peg p
      | isOccupied p b = show p ++ (separation p)
      | otherwise = "." ++ (separation p)
    separation p
      | isTriangularNum p = "\n"
      | otherwise = " "
    max_layers = 5

isTriangularNum :: Int -> Bool
isTriangularNum n = ((triangularNum $ layerNum n) == n)

isOccupied :: Int -> Board -> Bool
isOccupied pt b = pt `elem` b


-- Pretty-print a move.
print_move :: Move -> IO ()
print_move (from, over, to) =
   putStrLn $ "Jump a peg from position " ++ show from 
              ++ " over position " ++ show over
              ++ " to position " ++ show to ++ "."


-- Print the tally of ending pegs.
-- 6 lines.
print_ending_pegs :: Array Int Int -> IO ()
print_ending_pegs arr = Traversable.mapM (putStr . (" " ++) . show) arr
                        >> putStrLn ""
        

-- Pretty-print a solution.
-- 10 lines.
print_solution :: Solution -> IO ()
print_solution (starting, subs) = print_board starting
                                  >> mapM_ print_move_board subs

print_move_board :: (Move, Board) -> IO ()
print_move_board (move, board) = print_move move
                                 >> print_board board


----------------------------------------------------------------------
-- Solving the game.
----------------------------------------------------------------------

-- Return True if a move is valid on a particular board,
-- otherwise False.
-- 2 lines.
valid_move :: Board -> Move -> Bool
valid_move b (from, over, to) = (from, over, to) `elem` moves
                                && (isOccupied from b)
                                && (isOccupied over b)
                                && (not $ isOccupied to b)


-- Make a specific move on a specific board.
-- Assumes that the move is valid on the board.
-- 3 lines.
make_move :: Board -> Move -> Board
make_move board (from, over, to) = List.delete from
                                   $ List.delete over
                                   $ List.insert to board

-- Make all possible moves on a given board.
-- Return (move, board) pairs for the boards resulting from the moves.
-- 4 lines.  Uses monadic style (list monad).
make_moves :: Board -> [(Move, Board)]
make_moves b = zip valid_moves valid_boards
  where
    valid_boards = map (make_move b) valid_moves
    valid_moves = filter (valid_move b) moves


-- Return True if a board is a solution.
-- check if the board has only one piece left on it
is_solution :: Board -> Bool
is_solution b = (length b) == 1


-- Starting from a board, generate a list of lists of all the move/board
-- pairs that lead to a solution.  The original board is not part of the
-- lists.  8 lines.  Uses monadic style (list monad).
update :: Board -> [[(Move, Board)]]
update b
  | is_solution b = [[]]
  | otherwise = concat $ zipWith prepend_pair move_boards partial_solutions
    where
      partial_solutions :: [[[(Move, Board)]]]
      partial_solutions = map (update . snd) move_boards
      -- inclusion of filter solutions is seemingly uneeded
      move_boards :: [(Move, Board)]
      move_boards = make_moves b

{-
filter_solutions :: [[(Move, Board)]] -> [[(Move, Board)]]
filter_solutions [[]] = [[]]
filter_solutions sols = filter (is_solution . snd . last) sols
-}

prepend_pair :: (Move, Board) -> [[(Move, Board)]] -> [[(Move, Board)]]
-- mb -> move/board pair :: (Move, Board)
-- ps -> partial solutions :: [[(Move, Board)]]
prepend_pair mb ps = map (mb:) ps

-- Compute all possible game solutions starting from a given board.  
-- 1 line.
all_solutions :: Board -> [Solution]
all_solutions b = zip (repeat b) (update b)


-- Compute the number of solutions which end in each numbered peg.
-- Return an array of the count for each peg.
-- 8 lines.
count_ending_pegs :: [Solution] -> Array Int Int
count_ending_pegs sols = listArray (0, last_peg)
                         $ map (count sols) [0..last_peg]
  where
    -- the number of solutions ending in a given integer
    count :: [Solution] -> Int -> Int
    count sols n = length $ filter ((n ==) . last_move) sols
    last_move = head . snd . last . snd
    last_peg = 14


----------------------------------------------------------------------
-- Entry point.
----------------------------------------------------------------------

-- Compute all possible solutions to the triangle game puzzle.
-- Print out the number of solutions, the tally of ending pegs
-- and the first solution in move-by-move detail.
main :: IO ()
main = 
   let sols        = all_solutions starting_board
       nsols       = length sols
       sol1        = head $ sols
       ending_pegs = count_ending_pegs sols
       blank_line  = putStr "\n"
   in
   do putStrLn $ "Total number of solutions: " ++ show nsols
      blank_line
      putStrLn $ "Ending peg counts:"
      print_ending_pegs ending_pegs
      blank_line
      putStrLn "Detailed solution:"
      print_solution sol1
