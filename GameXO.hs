import Data.Maybe
import Data.Char
import System.IO.Unsafe
import System.Random

data OwO =  X|O deriving (Show,Eq)
type Space = Maybe OwO
type Board = [[Space]]
--type Position = (Int, Int)
--data GameResult = Win OwO| Die deriving (Show)
type GameState = (OwO, Board)

main::IO()
main =do
        putStrLn ("С кем будет игра Бот или Игрок")
        igr <- getLine
        if(igr == "Бот")
            then do gameBot (X,[[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing]])
            else do game (X,[[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing],[Nothing, Nothing, Nothing]])


gameBot::GameState -> IO()
gameBot sost | check (snd(sost)) (Just X) == 0 &&  check (snd(sost)) (Just O) == 0  && (checkSv (snd(sost))) == False = do 
                                                                                                                        if((fst sost) == X)
                                                                                                                                then do  hodIgr 1 (snd sost)
                                                                                                                                else do  hodBot varHod (snd sost)
          |checkSv (snd(sost)) == True = do putStrLn ("Ничья")
                                            vivDosk (snd sost)
          | check (snd(sost)) (Just X) == 1 = do putStrLn ("Выиграл X")
                                                 vivDosk (snd sost)
          | otherwise = do vivDosk (snd sost)
                           putStrLn ("Выиграл O")

game::GameState -> IO()
game sost | check (snd(sost)) (Just X) == 0 &&  check (snd(sost)) (Just O) == 0  && (checkSv (snd(sost))) == False = do 
                                                                                                                        if((fst sost) == X)
                                                                                                                                then do  hodIgr 0 (snd sost)
                                                                                                                                else do  hodIgr2 (snd sost)
          | check (snd(sost)) (Just X) == 1 = do putStrLn ("Выиграл X")
                                                 vivDosk (snd sost)
          |checkSv (snd(sost)) == True = do putStrLn ("Ничья")
                                            vivDosk (snd sost)
          | otherwise = do vivDosk (snd sost)
                           putStrLn ("Выиграл O")

------------------------------------------------------------------------------------------------------
helpchec:: Maybe OwO -> Maybe OwO -> Bool
helpchec owo uwu | owo == uwu = True
                 | otherwise = False

check::Board ->Maybe OwO -> Int
check board owo| (anys (helpchec owo) (board!!0)) || (anys (helpchec owo) (board!!1)) || (anys (helpchec owo) (board!!2)) ||( ((board!!0)!!0 == (board!!1)!!0 && (board!!1)!!0 == (board!!2)!!0) && ((board!!2)!!0 == owo)) || ( ((board!!0)!!1 == (board!!1)!!1 && (board!!1)!!1 == (board!!2)!!1) && ((board!!2)!!1 == owo)) || ( ((board!!0)!!2 == (board!!1)!!2 && (board!!1)!!2 == (board!!2)!!2) && ((board!!2)!!2 == owo)) || ( ((board!!0)!!0 == (board!!1)!!1 && (board!!1)!!1 == (board!!2)!!2) && ((board!!2)!!2 == owo)) || ( ((board!!0)!!2 == (board!!1)!!1 && (board!!1)!!1 == (board!!2)!!0) && ((board!!2)!!0 == owo)) = 1
               | otherwise = 0

helpchec1:: Maybe OwO -> Bool
helpchec1 owo | owo == (Just X) || owo == (Just O) = True
              | otherwise = False

checkSv::Board -> Bool
checkSv board | (anys (helpchec1) (board!!0)) && (anys (helpchec1) (board!!1)) && (anys (helpchec1) (board!!2)) = True
            | otherwise = False
------------------------------------------------------------------------------------------------------
anys::(a -> Bool) -> [a] -> Bool
anys func sp |length sp > 0 = do if(func(head sp))
                                        then do (anys func (tail sp))
                                        else do False
             | length sp == 0 = True  

vivOwO:: Maybe OwO -> String
vivOwO Nothing = " "
vivOwO xs = show (fromJust xs)

vivDosk:: Board -> IO()
vivDosk dosk = do  putStrLn((vivOwO ((dosk!!0)!!0))++"|"++(vivOwO ((dosk!!0)!!1))++"|"++(vivOwO ((dosk!!0)!!2)))
                   putStrLn("__________")
                   putStrLn((vivOwO ((dosk!!1)!!0))++"|"++(vivOwO ((dosk!!1)!!1))++"|"++(vivOwO ((dosk!!1)!!2)))
                   putStrLn("__________")
                   putStrLn((vivOwO ((dosk!!2)!!0))++"|"++(vivOwO ((dosk!!2)!!1))++"|"++(vivOwO ((dosk!!2)!!2)))

prov::(Int,Int) -> Board -> Bool
prov pos dosk | ((dosk!!((fst pos) - 1))!!((snd pos)-1)) == Nothing = True
              | otherwise = False

repla:: [a] -> Int -> a -> [a]
repla xs i x = take i xs ++ [x] ++ drop (i + 1) xs

zamena::(Int,Int) -> Board -> OwO -> Board
zamena pol dosk el = repla dosk ((fst pol)-1) (repla (dosk!!((fst pol)-1)) ((snd pol)-1) (Just el))

------------------------------------------------------------------------------------------------------

hodIgr::Int -> Board -> IO()
hodIgr st dosk = do putStrLn("Ход X. Поле в данный момент:")
                    vivDosk dosk
                    putStrLn("Ведите позицию и введите X Y где x строка y столбец")
                    let str = words (unsafePerformIO getLine)
                        pos = ((digitToInt((str!!0)!!0)),(digitToInt((str!!1)!!0)))
                    if(prov pos dosk)
                        then do let newDosk = zamena pos dosk X
                                if(st == 0)
                                    then do game(O,newDosk)
                                    else do gameBot (O,newDosk)
                        else do 
                                putStrLn("ПОЗИЦИЯ УЖЕ ЗАНЯТА")
                                hodIgr st dosk


--------------------------------------------------------------------------------------------------------

hodIgr2::Board -> IO()
hodIgr2 dosk = do   putStrLn("Ход O. Поле в данный момент:")
                    vivDosk dosk
                    putStrLn("Ведите позицию и введите (X,Y) где x строка y столбец")
                    let str = words (unsafePerformIO getLine)
                        pos =  ((digitToInt((str!!0)!!0)),(digitToInt((str!!1)!!0)))
                    if(prov pos dosk)
                        then do let newDosk = zamena pos dosk O
                                game(X,newDosk)
                        else do 
                                putStrLn("ПОЗИЦИЯ УЖЕ ЗАНЯТА")
                                hodIgr2 dosk


--------------------------------------------------------------------------------------------------------
hodBot::[(Int,Int)] -> Board -> IO()
hodBot hod dosk = do   
                    let str = words (unsafePerformIO getLine)
                        ch = unsafePerformIO (drawInt ((length hod)-1))
                        pos =  hod!!ch
                    if(prov pos dosk)
                        then do let newDosk = zamena pos dosk O
                                gameBot (X,newDosk)
                        else do 
                                hodBot (del hod ch) dosk 

del:: [a] -> Int -> [a]
del xs i = take i xs ++ drop (i + 1) xs

drawInt :: Int -> IO Int
drawInt c = do
               g <- newStdGen 
               return(fst(randomR (0,c) g))

varHod = [(2,2),(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]
--------------------------------------------------------------------------------------------------------