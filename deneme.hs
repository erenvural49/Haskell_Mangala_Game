import System.IO
import Data.List (intercalate, isInfixOf)

-- Rakip oyuncunun kutusunu atlamak için sonraki delik indeksini hesaplar.
-- Oyuncu 1 için indeksler: 0..5 (kendi delikleri), 6 (kutusu), 7..12 (rakip delikleri). 13'ü (P2 kutusu) atlar ve 0'a döner.
-- Oyuncu 2 için indeksler: 7..12 (kendi delikleri), 13 (kutusu), 0..5 (rakip delikleri). 6'yı (P1 kutusu) atlar ve 7'ye döner.
nextHole :: Int -> Int -> Int
nextHole 1 12 = 0       -- Oyuncu 1, Rakip 6. delikten sonra kendi 1. deliğine döner (Rakip P2 Kutusunu (13) atlar)
nextHole 1 curr = curr + 1

nextHole 2 5 = 7        -- Oyuncu 2, Rakip 6. delikten sonra kendi 1. deliğine döner (Rakip P1 Kutusunu (6) atlar)
nextHole 2 13 = 0       -- Oyuncu 2 kendi kutusundan sonra ilk rakip deliğine döner
nextHole 2 curr = curr + 1

-- Verilen indeksteki elemanı güncelleyerek yeni bir liste döndüren yardımcı fonksiyon.
updateList :: Int -> (a -> a) -> [a] -> [a]
updateList idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

-- Elimizdeki taşları dağıtma işlemini yapan özyinelemeli fonksiyon.
-- Dağıtım bitene kadar her "nextHole" hücresine 1 taş ekler.
distribute :: Int -> Int -> Int -> [Int] -> (Int, [Int])
distribute player hand currentHole board
    | hand == 0 = (currentHole, board)
    | otherwise =
        let nHole = nextHole player currentHole
            newBoard = updateList nHole (+ 1) board
        in distribute player (hand - 1) nHole newBoard

-- Oyuncunun son taşı kendi boş deliğine gelirse, karşıdaki rakip taşlarını yeme durumunu (Capture) kontrol eder.
checkCapture :: Int -> Int -> [Int] -> [Int]
checkCapture player lastHole b
    | player == 1 && lastHole >= 0 && lastHole <= 5 && b !! lastHole == 1 =
        let opp = 12 - lastHole -- Karşı deliğin indeksi
        in if b !! opp > 0      -- Eğer karşı delikte taş varsa
           then let captured = (b !! lastHole) + (b !! opp)
                in updateList 6 (+ captured) $ updateList lastHole (const 0) $ updateList opp (const 0) b
           else b
    | player == 2 && lastHole >= 7 && lastHole <= 12 && b !! lastHole == 1 =
        let opp = 12 - lastHole -- Karşı deliğin indeksi
        in if b !! opp > 0      -- Eğer karşı delikte taş varsa
           then let captured = (b !! lastHole) + (b !! opp)
                in updateList 13 (+ captured) $ updateList lastHole (const 0) $ updateList opp (const 0) b
           else b
    | otherwise = b

-- Tahtanın istenilen (istenen çıktılardaki) formata dönüştürülmesini sağlar.
formatState :: [Int] -> String
formatState board =
    let p1box = board !! 6
        p2box = board !! 13
        -- Üst ve alt sıradaki deliklerin taş sayılarını alırız.
        topHoles = reverse (take 6 (drop 7 board))
        botHoles = take 6 board

        -- Hücre içlerini boşluklarla hizalamak için fonksiyon (2 basamaklı sayılarda kaymayı önler)
        formatCell 0 = "   "
        formatCell n = if n < 10 then " " ++ show n ++ " " else " " ++ show n

        topHolesStr = intercalate "|" (map formatCell topHoles)
        botHolesStr = intercalate "|" (map formatCell botHoles)

        -- P2 kutusunun olup olmamasına göre soldaki boşlukları ayarlar
        prefixTop = if p2box > 0 then show p2box ++ " " else ""
        prefixBot = if p2box > 0 then replicate (length prefixTop) ' ' else ""

        -- P1 kutusu varsa, alt sıranın sağına yazdırır
        suffixBot = if p1box > 0 then " " ++ show p1box else ""

    in prefixTop ++ "|" ++ topHolesStr ++ "|\n" ++ prefixBot ++ "|" ++ botHolesStr ++ "|" ++ suffixBot

-- Herhangi bir oyuncunun tarafındaki tüm taşların bitip bitmediğini kontrol eder.
isGameOver :: [Int] -> Bool
isGameOver b = (sum (take 6 b) == 0) || (sum (take 6 (drop 7 b)) == 0)

-- Oyun sonuçlarına göre kazananı ekrana formatlı olarak yazdırmak için yardımcı fonksiyon.
checkWinner :: [Int] -> String
checkWinner b =
    let p1 = b !! 6
        p2 = b !! 13
    in if p1 > p2 then "Player 1 won the game and the number of stones both players have: Player 1: " ++ show p1 ++ ", Player 2: " ++ show p2
       else if p2 > p1 then "Player 2 won the game and the number of stones both players have: Player 1: " ++ show p1 ++ ", Player 2: " ++ show p2
       else "It's a tie! stones: Player 1: " ++ show p1 ++ ", Player 2: " ++ show p2

-- Oyun bittiğinde kalan tüm taşları, tarafında taş biten oyuncunun kutusuna ekler ve kazananı yazdırır.
handleEndGame :: [Int] -> IO ()
handleEndGame b = do
    let p1Sum = sum (take 6 b)
        p2Sum = sum (take 6 (drop 7 b))
        (finalB, winnerStr) = if p1Sum == 0
                              -- Player 1'in taşları bittiyse Player 2'nin kalan taşlarını Player 1 alır.
                              then let newP1Box = (b !! 6) + p2Sum
                                       newB1 = updateList 6 (const newP1Box) b
                                       newB2 = map (\(i, x) -> if i >= 7 && i <= 12 then 0 else x) (zip [0..] newB1)
                                   in (newB2, checkWinner newB2)
                              -- Player 2'nin taşları bittiyse Player 1'in kalan taşlarını Player 2 alır.
                              else let newP2Box = (b !! 13) + p1Sum
                                       newB1 = updateList 13 (const newP2Box) b
                                       newB2 = map (\(i, x) -> if i >= 0 && i <= 5 then 0 else x) (zip [0..] newB1)
                                   in (newB2, checkWinner newB2)
    putStrLn (formatState finalB)
    putStrLn ""
    putStrLn winnerStr


-- Oyuncunun seçtiği hamleyi işler ve arka planda kural kontrollerini yapar.
playTurnHelper :: Int -> Int -> [Int] -> IO ()
playTurnHelper player holeIdx board = do
    let stones = board !! holeIdx
    if stones == 0 then do
        putStrLn "Invalid choice: hole is empty. Try again."
        promptTurn player False board
    else do
        -- Kural 9: Seçilen delikte tek taş varsa o taşı yandaki ardışık kuyuya kaydırır. Değilse 1 taş bırakıp kalanı dağıtır.
        let (hand, boardAfterTake) = if stones == 1
                                     then (1, updateList holeIdx (const 0) board)
                                     else (stones - 1, updateList holeIdx (const 1) board)
        
        -- Taşları dağıtıyoruz
        let (lastHole, boardAfterDist) = distribute player hand holeIdx boardAfterTake

        -- Son taş boş kendi kuyumuza geldiyse rakibin taşlarını ele geçiriyoruz (Capture)
        let capturedBoard = checkCapture player lastHole boardAfterDist

        -- Hamle sonundaki güncel tahta durumunu yazdır
        putStrLn (formatState capturedBoard)

        if isGameOver capturedBoard then do
            handleEndGame capturedBoard
        else do
            -- Kural 7v: Eğer oyuncunun elindeki son taş kendi kutusuna (hazinesine) atıldıysa oyuncu bir el daha oynar.
            let isLastBox = (player == 1 && lastHole == 6) || (player == 2 && lastHole == 13)
            if isLastBox then do
                -- Kullanıcı isteğine göre 'Tekrar oynama hakkı kazandınız' mesajını kaldırdık. Direkt yeni hamleye sorar.
                promptTurn player True capturedBoard
            else do
                promptTurn (if player == 1 then 2 else 1) False capturedBoard

-- Kullanıcıdan girdi alır ve girdiyi oyun mantığına uygular.
promptTurn :: Int -> Bool -> [Int] -> IO ()
promptTurn player isNewTurn board = do
    if isNewTurn then
        putStrLn $ "User " ++ show player ++ " Enter a new hole number:"
    else
        putStrLn $ "User " ++ show player ++ " Enter hole number:"
    line <- getLine
    -- Satırın başındaki boşlukları temizleyerek güvenli bir şekilde Int (sayı) tipine çevir.
    let cleanLine = unwords (words line)
    let parsed = reads cleanLine :: [(Int, String)]
    case parsed of
        (n, ""):_ -> if n >= 1 && n <= 6
                     then do
                         -- Seçilen numaranın tahtadaki indeksine eşlenmesi
                         let idx = if player == 1 then n - 1 else 6 + n
                         if board !! idx == 0
                             then do
                                 putStrLn "Invalid choice: hole is empty. Try again."
                                 promptTurn player isNewTurn board
                             else playTurnHelper player idx board
                     else do
                         putStrLn "Invalid hole number. Must be 1-6."
                         promptTurn player isNewTurn board
        _ -> do
            putStrLn "Invalid input. Please enter a number."
            promptTurn player isNewTurn board

-- Programın başladığı nokta. Oyuncuyu sorar, ilk formatı çıkartır ve başlangıç durumunu yaratıp döngüye girer.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Select the first player: "
    line <- getLine
    let firstPlayer = if "2" `isInfixOf` line then 2 else 1
    
    putStrLn "\n        GAME MANGALA"
    -- Her delikte başlangıçta 4 taş bulunur, hazineler 0 ile başlar.
    let initialBoard = replicate 6 4 ++ [0] ++ replicate 6 4 ++ [0]
    putStrLn (formatState initialBoard)
    putStrLn ""
    
    promptTurn firstPlayer False initialBoard
