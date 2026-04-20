import System.IO
import Data.List (intercalate, isInfixOf)

-- 1. VERİ YAPISI (Custom Data Type)
-- Her oyuncunun kendine ait bir listesi ([Int]) ve haznesi (Int) var.
data Board = Board {
    p1Holes :: [Int],
    p1Box   :: Int,
    p2Holes :: [Int],
    p2Box   :: Int
} deriving (Show)

data Player = Player1 | Player2 deriving (Show, Eq)

-- 2. BAŞLANGIÇ DURUMU
initialBoard :: Board
initialBoard = Board (replicate 6 4) 0 (replicate 6 4) 0

-- 3. EKRANA BASMA MANTIĞI
displayBoard :: Board -> IO ()
displayBoard b = do
    putStrLn "\n      GAME MANGALA"
    
    -- Üst Etiketler (Player 2 için H6'dan H1'e)
    -- printf ile boşlukları hizalıyoruz
    putStrLn "      H6  H5  H4  H3  H2  H1"
    putStrLn "-----------------------------"
    
    -- Player 2 Satırı: Hazne solda, delikler 5'ten 0'a doğru (ters)
    printf "%2d | %d | %d | %d | %d | %d | %d |\n" 
           (p2Box b) (p2Holes b !! 5) (p2Holes b !! 4) (p2Holes b !! 3) 
           (p2Holes b !! 2) (p2Holes b !! 1) (p2Holes b !! 0)
    
    -- Player 1 Satırı: Delikler 0'dan 5'e doğru (düz), hazne sağda
    printf "   | %d | %d | %d | %d | %d | %d | %2d\n" 
           (p1Holes b !! 0) (p1Holes b !! 1) (p1Holes b !! 2) 
           (p1Holes b !! 3) (p1Holes b !! 4) (p1Holes b !! 5) (p1Box b)
           
    putStrLn "-----------------------------"
    -- Alt Etiketler (Player 1 için H1'den H6'ya)
    putStrLn "      H1  H2  H3  H4  H5  H6"

main :: IO ()
main = displayBoard initialBoard