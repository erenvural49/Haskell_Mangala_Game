-- CSE2260 - Principles of Programming Languages
-- Project 1: Mangala Game
-- Group Members:
-- 150125823 Cemal Mert Özkan
-- 150124020 Halil Diken
-- 150124075 Eren Vural

import System.IO
import Data.List (intercalate, isInfixOf)

-- Finds the next index, skipping the opponent's store
getNextIndex :: Int -> Int -> Int
getNextIndex 1 12 = 0
getNextIndex 1 i  = i + 1
getNextIndex 2 5  = 7
getNextIndex 2 13 = 0
getNextIndex 2 i  = i + 1

-- Updates a specific index in the list
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

-- Handles the distribution of stones
moveStones :: Int -> Int -> Int -> [Int] -> (Int, [Int])
moveStones _ 0 current b = (current, b)
moveStones player stones current b = 
    let next = getNextIndex player current
        newBoard = updateAt next (+1) b
    in moveStones player (stones - 1) next newBoard

-- Rule 10: Captures stones if the last stone lands in an empty hole
applyCapture :: Int -> Int -> [Int] -> [Int]
applyCapture player lastIdx b
    | player == 1 && lastIdx >= 0 && lastIdx <= 5 && b !! lastIdx == 1 =
        let oppIdx = 12 - lastIdx
        in if b !! oppIdx > 0 
           then updateAt 6 (+(1 + b !! oppIdx)) $ updateAt lastIdx (const 0) $ updateAt oppIdx (const 0) b
           else b
    | player == 2 && lastIdx >= 7 && lastIdx <= 12 && b !! lastIdx == 1 =
        let oppIdx = 12 - lastIdx
        in if b !! oppIdx > 0
           then updateAt 13 (+(1 + b !! oppIdx)) $ updateAt lastIdx (const 0) $ updateAt oppIdx (const 0) b
           else b
    | otherwise = b

-- Formats the board state for display
displayBoard :: [Int] -> String
displayBoard b =
    let p2holes = reverse (take 6 (drop 7 b))
        p1holes = take 6 b
        p1box = b !! 6
        p2box = b !! 13
        
        -- Empty if 0
        pad 0 = "   "
        pad n = if n < 10 then " " ++ show n ++ " " else " " ++ show ng
        fmtBox 0 = "  "
        fmtBox n = if n < 10 then " " ++ show n else show n
    
        p2Str = fmtBox p2box
        p1Str = fmtBox p1box
        topRow = p2Str ++ " |" ++ intercalate "|" (map pad p2holes) ++ "|"
        bottomRow = "   |" ++ intercalate "|" (map pad p1holes) ++ "| " ++ p1Str
    in "\n" ++ topRow ++ "\n" ++ bottomRow ++ "\n"

-- Check if any side is empty
checkGameOver :: [Int] -> Bool
checkGameOver b = sum (take 6 b) == 0 || sum (take 6 (drop 7 b)) == 0

-- Handles game end, collects remaining stones 
finalizeGame :: [Int] -> IO ()
finalizeGame b = do
    let p1Remaining = sum (take 6 b)
        p2Remaining = sum (take 6 (drop 7 b))
        finalB = if p1Remaining == 0 
                 then updateAt 6 (+p2Remaining) [if i >= 7 && i <= 12 then 0 else x | (i,x) <- zip [0..] b]
                 else updateAt 13 (+p1Remaining) [if i <= 5 then 0 else x | (i,x) <- zip [0..] b]
        p1Final = finalB !! 6
        p2Final = finalB !! 13
    putStrLn (displayBoard finalB)
    if p1Final > p2Final 
        then putStrLn $ "Player 1 won! Player 1: " ++ show p1Final ++ ", Player 2: " ++ show p2Final
        else if p2Final > p1Final 
            then putStrLn $ "Player 2 won! Player 1: " ++ show p1Final ++ ", Player 2: " ++ show p2Final
            else putStrLn "The game ended in a tie!"

-- Main turn loop
playTurn :: Int -> Bool -> [Int] -> IO ()
playTurn player isExtraTurn board = do
    let prompt = if isExtraTurn then "Enter a new hole number: " else "Enter hole number: "
    putStr $ "User " ++ show player ++ " " ++ prompt
    input <- getLine
    let holeNum = (read input :: Int)
    let idx = if player == 1 then holeNum - 1 else holeNum + 6
    
    if holeNum < 1 || holeNum > 6 || board !! idx == 0 
       then putStrLn "Invalid move, try again.\n" >> playTurn player isExtraTurn board
       else do
           let count = board !! idx
           -- Single stone check
           let (inHand, bAfterPick) = if count == 1 
                                      then (1, updateAt idx (const 0) board) 
                                      else (count - 1, updateAt idx (const 1) board)
           let (lastIdx, bAfterDist) = moveStones player inHand idx bAfterPick
           let bAfterCapture = applyCapture player lastIdx bAfterDist
           
           -- Print board with spaces for better visibility
           putStrLn (displayBoard bAfterCapture)
           
           if checkGameOver bAfterCapture 
               then finalizeGame bAfterCapture
               else do
                    -- Extra turn if lands in box
                   let extra = (player == 1 && lastIdx == 6) || (player == 2 && lastIdx == 13)
                   if extra 
                       then playTurn player True bAfterCapture
                       else playTurn (if player == 1 then 2 else 1) False bAfterCapture

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Select the first player: "
    choice <- getLine
    let p = if "2" `isInfixOf` choice then 2 else 1
    putStrLn "\n        GAME MANGALA"
    -- Initial board setup
    let startBoard = replicate 6 4 ++ [0] ++ replicate 6 4 ++ [0]
    putStrLn (displayBoard startBoard)
    playTurn p False startBoard