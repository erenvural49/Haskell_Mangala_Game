import Text.Printf (printf)

-- 1. VERİ YAPISI (Custom Data Type)
-- Her oyuncunun kendine ait bir listesi (hole için list (H)) ([Int]) ve haznesi (Box için int (H)) (Int) var. -- 
data Board = Board {
    p1Holes :: [Int],
    p1Box   :: Int,
    p2Holes :: [Int],
    p2Box   :: Int
} deriving (Show)

data Player = Player1 | Player2 deriving (Show, Eq)
-- (Kendi datatype için özellik yükleme (H))

-- 2. BAŞLANGIÇ DURUMU
initialBoard :: Board
initialBoard = Board (replicate 6 4) 0 (replicate 6 4) 0
-- Sırayla p1holes (6 tane 4 ), p1Box(0) vs eşleştir (H)

-- 3. EKRANA BASMA MANTIĞI
displayBoard :: Board -> IO ()
displayBoard b = do
    putStrLn "\n      GAME MANGALA"
    
    -- Üst Etiketler (Player 2 için H6'dan H1'e)
    -- printf ile boşlukları hizalıyoruz
    putStrLn "      H6  H5  H4  H3  H2  H1"
    putStrLn "-----------------------------"
    
    -- Player 2 Satırı: Hazne solda, delikler 5'ten 0'a doğru (ters)
    printf "%2d | %2d | %2d | %2d | %2d | %2d | %2d |\n" 
           (p2Box b) (p2Holes b !! 5) (p2Holes b !! 4) (p2Holes b !! 3) 
           (p2Holes b !! 2) (p2Holes b !! 1) (p2Holes b !! 0)
    
    -- Player 1 Satırı: Delikler 0'dan 5'e doğru (düz), hazne sağda
    printf "   | %2d | %2d | %2d | %2d | %2d | %2d | %2d\n" 
           (p1Holes b !! 0) (p1Holes b !! 1) (p1Holes b !! 2) 
           (p1Holes b !! 3) (p1Holes b !! 4) (p1Holes b !! 5) (p1Box b)
           
    putStrLn "-----------------------------"
    -- Alt Etiketler (Player 1 için H1'den H6'ya)
    putStrLn "      H1  H2  H3  H4  H5  H6"
    -- Halil : %2d olarak hepsini değiştirdim.

-- 4. Oynanış Döngüsü (H)

-- Oyun bitti mi kontrolü
isGameOver :: Board->Bool
isGameOver b = sum(p1Holes b) == 0 || sum (p2Holes b) == 0 

-- İlk oyuncu kim olacak input alma
firstPlayer :: IO Player
firstPlayer = do
    putStrLn "Who will start the game player1 or player2 ? "
    input <- getLine
    if input == "1" || input == "player1"
        then do
            putStrLn "Player 1 selected"
            return Player1
        else if input == "2" || input == "player2"
            then do
                putStrLn "Player 2 selected"   
                return Player2
            else do
                putStrLn "Invalid input. Please enter 1 or 2"
                firstPlayer -- Call again    
-- 5.

gameLoop :: Board -> Player -> IO()
gameLoop b p = do
    if isGameOver b
        then do
            putStrLn "Game Over" 
        -- Total scoru hesaplamalıyız
        else do
            displayBoard b

            let playerStr = if p== Player1 then "Player 1" else "Player 2"
            putStrLn(playerStr ++ "Please enter hole number")
            input <- getLine
            let holeNum = read input :: Int -- input'u sayıya çevir
            let holeIdx = holeNum - 1 

            let currentHoles = if p == Player1 then p1Holes b else p2Holes b

            if holeNum < 1 || holeNum > 6 || currentHoles !! holeIdx == 0

                then do
                    putStrLn "Invalid choose ! Please enter valid choose " -- taş olmayanı seçti diyelim.
                    gameLoop b p --

                else do  -- taş olanı seçtiğinde artık oynayışını yapmak gerek       

main :: IO ()
-- main = displayBoard initialBoard || Güncelledim burayı
main = do
    player <- firstPlayer
    displayBoard initialBoard
