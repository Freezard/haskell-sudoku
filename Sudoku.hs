{-
Introduction to Functional Programming
Lab 3
Jonas Einarsson (einarssj@student.chalmers.se)
Mattias Majetic (gusmajem@student.gu.se) 
-}



module Sudoku where

import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Test.QuickCheck

-------------------------------------------------------------------------
example :: Sudoku
example = Sudoku
	[ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
	, [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
	, [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
	, [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
	, [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
	, [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
	, [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
	, [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
	, [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
	]


example2 :: Sudoku
example2 = Sudoku
	[ [Just 3, Just 6, Just 5,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
	, [Just 1,Just 5, Just 2,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
	, [Just 6,Just 3,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
	, [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
	, [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
	, [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
	, [Nothing,Nothing,Nothing, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
	, [Nothing,Nothing, Nothing, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
	, [Nothing,Nothing,Nothing, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
	]-------------------------------------------------------------------------


data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Eq )

instance Show Sudoku where
    show = showSudoku

-- ASSIGNMENT A
---------------

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s =    (length (rows s)) == 9 && 
        and [length row == 9 | row <- (rows s)] &&
        and [f row | row <- (rows s)] where
            f row = and [f' cell | cell <- row] where
                f' Nothing = True
                f' (Just n) = n <= 9 && n>=0

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [Nothing `notElem` row | row <- (rows s)]

-------------------------------------------------------------------------

-- ASSIGNMENT B
---------------

-- showSudoku
showSudoku s = concat [buildSudokuRow row ++ "\n" | row <- (rows s)] where    
    buildSudokuRow r = [buildSudokuCell cell | cell <- r] where
       buildSudokuCell Nothing  = '.'
       buildSudokuCell (Just n) = intToDigit n


-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn (showSudoku s)

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                  filetext <- readFile fp
                  
                  return (parseSudoku filetext)

-- parseSudoku does the grunt work of turning a string into a Sudoku                  
parseSudoku :: String -> Sudoku
parseSudoku s 
    | not (isSudoku (parse s)) = error "parseSudoku: Not a sudoku."
    | otherwise                = parse s where
        parse = 
            Sudoku 
          . (map (map f)) 
          . lines where
                f '.' = Nothing
                f ch  = Just (digitToInt ch)
                
                

-------------------------------------------------------------------------

-- ASSIGNMENT C
---------------

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku


-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [
                  (9, return Nothing), 
                  (1, do n <- choose(1,9)
                         return (Just n))
                 ]
                         

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------

-- ASSIGNMENT D
---------------

type Block = [Maybe Int]

-- Checks for duplicates
isOkayBlock :: Block -> Bool
isOkayBlock []           = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs)       = (notElem x xs) && (isOkayBlock xs)
                
-- Extracts all 3*9 blocks from a Sudoku
blocks :: Sudoku -> [Block]
blocks s = (rows s) ++ (transpose (rows s)) ++ squares s where
    squares s = [square (x,y) s | y <- [0..2], x <- [0..2]]

-- Get column x from sudoku s (used in Assignment X below)
column :: Int -> Sudoku -> Block
column x s = transpose (rows s) !! x

-- Get row x from sudoku s (used in Assignment X below)
row :: Int -> Sudoku -> Block
row x s = (rows s) !! x

-- Get square (x,y), 0 <= x,y <= 2 from sudoku s
square :: (Int, Int) -> Sudoku -> Block
square (x,y) s = 
      concat
      $ [take 3 (drop (x*3) row) | row <- take 3 (drop (y*3) (rows s))]


isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- (blocks s)]


prop_blocks :: Sudoku -> Bool
prop_blocks s = ((length bl) == 3*9) && 
                and [(length b) == 9 | b <- bl] where
                    bl = blocks s

-------------------------------------------------------------------------

-- ASSIGNMENT E
---------------

-- (Row, Col)
type Pos = (Int, Int)

-- Return a  blank pos in Sudoku
blank :: Sudoku -> Pos
blank = head . allBlanks


-- Indexing for Sudokus (and other list-lists)
(!!!) :: [[a]] -> Pos -> a
(!!!) l (y,x) = (l !! y) !! x

-- Finds all blank spaces in a sudoku
allBlanks :: Sudoku -> [Pos]
allBlanks s = [ (y,x) | y <- [0..8], x <- [0..8], ((rows s) !!! (y,x)) == Nothing]


-- The replace operator
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, x)  
    | i < 0 || i > (length l)-1 = l
    | otherwise                 = (take i l) ++ [x] ++ (drop (i+1) l)

-- Updates position in Sudoku
-- Note reversed order in the pos to follow specification in lab!
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (y,x) val = Sudoku $
                     take y (rows s) ++ 
                     [row !!= (x, val)] ++ 
                     drop (y+1) (rows s) where
                        row = (rows s) !! y
                                          


-- (!!=): The length should be the same after replacing an element
prop_replaceoplength :: [a] -> (Int, a) -> Bool
prop_replaceoplength s (i,x) = length s == length (s !!= (i, x))


-------------------------------------------------------------------------

-- ASSIGNMENT F
---------------

-- Original "naive" solver
solve :: Sudoku -> Maybe Sudoku
solve = solveEx blank (\s -> Nothing)

-- A general sudokusolver which takes a blank-function and a
-- propagate-function and tries to solve the given sudoku.
solveEx :: (Sudoku -> Pos) -> (Sudoku -> Maybe Sudoku) -> Sudoku -> Maybe Sudoku
solveEx blankf propagatef s
  | not (isOkay s)        = Nothing
  | isSolved s            = Just s
  | propagated /= Nothing = solveEx blankf propagatef (fromJust propagated) 
  | otherwise             = listToMaybe solutions
    where
      propagated = propagatef s
      solutions = [ fromJust sol | n <- [1..9], 
                          let sol = solveEx blankf propagatef (update s (blankf s) (Just n)),
                          sol /= Nothing]                          
                          

-- Note: uses the optimized solveXY                          
readAndSolve :: FilePath -> IO()
readAndSolve fp = do
                   s <- readSudoku fp
                   putStrLn "Original:"
                   printSudoku s
                   putStrLn "Solution:"
                   printSudoku (fromJust (solveXY s))

-- Checks if s1 is a solution of s2
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = 
    isOkay s1 
    && isSolved s1
    && and [ a == b || b == Nothing | 
             (a,b) <- zip (concat (rows s1)) (concat (rows s2)) ]


-- Note: Uses the optimized solveXY, run with fewerChecks if you
-- do not like to wait...
prop_solveSound :: Sudoku -> Bool
prop_solveSound s 
    | solution == Nothing = True
    | otherwise           = isSolutionOf (fromJust solution) s where
                              solution = solveXY s

fewerCheck prop = check defaultConfig{ configMaxTest = 10 } prop

-------------------------------------------------------------------------

-- ASSIGNMENT X
---------------

-- We made two attempts of which one is somewhat successful.
-- The second attempt, blankX2, minimizes a "score" calculated
-- from the number of free spaces in rows, columns and squares.

-- The first attempt would be smarter (see comments below) but
-- the implementation is very slow so we are still working on it..

-- SolveX and SolveXY uses the blankX2 implementation, solveXY
-- also uses the propagation from assgn. Y (see below).

solveX = solveEx blankX2 (\s -> Nothing)      
solveXY = solveEx blankX2 propagate


-- Return a better blank pos in Sudoku
blankX2 s = 
      snd
      . minimum $ 
      [(score p, p) | p <- allBlanks s] where
        score (y,x) = rowScore y + colScore x + sqScore (div x 3, div y 3)
        rowScore r = blanksInBlock (row r s)
        colScore c = blanksInBlock (column c s)
        sqScore sq = blanksInBlock (square sq s)
        
        
-- Return a better blank pos in Sudoku
-- Chooses blank in the Block by this:
--  - pick the block with the least number of free spots
--  - pick the free spot in this block which has the least
--    free spots in the corresponding row, column and square
blankX :: Sudoku -> Pos
blankX s
  | bb < 9            = chooseInRow s (bb)
  | bb < 18           = chooseInColumn s (bb - 9)
  | otherwise         = chooseInSquare s (bb - 18) where
      bb = bestBlock s
      
      -- choose best spot in row y
      chooseInRow s y = 
        fst 
        . minimumBy compBySnd $
        [ ((y,x), val) |  x <- [0..8], 
                          let val = val' x y s,
                          (rows s) !!! (y,x) == Nothing]
         where
           val' x y s = blanksInBlock (column x s) + 
                        blanksInBlock (square (div x 3, div y 3) s)
      
      -- choose best spot in col x
      chooseInColumn s x = 
        fst 
        . minimumBy compBySnd $
        [ ((y,x), val) | y <- [0..8], 
                         let val = val' x y s,
                         (rows s) !!! (y,x) == Nothing]
        where
          val' x y s = blanksInBlock (row y s) + 
                       blanksInBlock (square (div x 3, div y 3) s)
      
      -- choose best spot in square n
      -- squares numbered from top-left to bottom-right
      chooseInSquare s n =  
        fst 
        . minimumBy compBySnd $
        [ ((gy,gx), val) |    x <- [0..2], 
                              y <- [0..2],
                              let gx = (x + 3*(mod n 3)),
                              let gy = (y + 3*(div n 3)),
                              let val = val' gx gy s,
                              ((rows s) !!! (gy,gx)) == Nothing] where
                                  val' x y s = blanksInBlock (column x s) + 
                                               blanksInBlock (row y s)

      -- Finds the block with the minimum amount of free spaces > 0
      bestBlock = fst
                  . minimumBy compBySnd
                  . filter (\p -> snd p > 0)
                  . blanksInBlocks
  
  
-- Compares a tuple by the second value, for use with minimumBy
compBySnd :: Ord b => (a, b) -> (c, b) -> Ordering
compBySnd (_,x) (_,y) | x < y     = LT
                      | x == y    = EQ
                      | otherwise = GT


-- This function calculates the number of free spaces in the given blocks
-- and zips it with the index
blanksInBlocks :: Sudoku -> [(Int,Int)]
blanksInBlocks s = [ (n, blanksInBlock b) | (b,n) <- zip (blocks s) [0..] ]
                        
blanksInBlock :: Block -> Int
blanksInBlock bl = length (filter (== Nothing) bl)


-------------------------------------------------------------------------

-- ASSIGNMENT Y
---------------

-- SolveY uses the naive blank function but adds propagation.
solveY = solveEx blank propagate      

-- Propagate sudoku
-- Returns Nothing if not able to propagate, otherwise
-- 1) finds the first block with only 1 free spot
-- 2) calculates the index in block and corresponding value
propagate :: Sudoku -> Maybe Sudoku
propagate s | length availBlocks == 0 = Nothing
            | otherwise = propagateBlock s (fst $ head availBlocks) where
    availBlocks = filter (\p -> snd p == 1) (blanksInBlocks s) 
    propagateBlock s n | n < 9     = Just (propagateRow s n)
                       | n < 18    = Just (propagateColumn s (n-9))
                       | otherwise = Just (propagateSquare s (n-18)) where
        idx = missingInBlockIndex
        val = missingInBlock
        propagateRow s k    = update s (k,idx (row k s)) (Just (val (row k s)))
        propagateColumn s k = update s (idx (column k s),k) (Just (val (column k s)))
        propagateSquare s k = update s (y, x) (Just (val sq)) where
            sq = square (mod k 3, div k 3) s
            y  = 3*(div k 3) + (div (idx sq) 3)
            x  = 3*(mod k 3) + (mod (idx sq) 3)

-- Finds the index of the first "Nothing" in a block
missingInBlockIndex b = 
    snd $
    head $ 
    filter (\p -> fst p == Nothing) $
    zip b [0..8]

-- Calculates the value that should be instead of the "Nothing" in a block
missingInBlock :: Block -> Int
missingInBlock b = maxInBlock - (foldl (+) 0 (map (fromMaybe 0) b)) where
    maxInBlock = foldl (+) 0 [1..9]

