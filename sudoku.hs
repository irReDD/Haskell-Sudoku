-- Генериране и решаване на Судоку
--
-- Под дъска за судоку разбираме 9 списъка от списъци със 9 Int-a
--
-- пример: board = [[5,3,0,0,7,0,0,0,0],
--                  [6,0,0,1,9,5,0,0,0],
--                  [0,9,8,0,0,0,0,6,0],
--                  [8,0,0,0,6,0,0,0,3],
--                  [4,0,0,8,0,3,0,0,1],
--                  [7,0,0,0,2,0,0,0,6],
--                  [0,6,0,0,0,0,2,8,0],
--                  [0,0,0,4,1,9,0,0,5],
--                  [0,0,0,0,8,0,0,7,9]]
--
--
-- Генериране на дъска за решаване:
--   > genBoard (трудност)
--      Трудността е колко празни елементи да има на дъската: от 0 (генериране на решена дъска)
--      до 69 (генериране на дъска със само 11 елемента). По-голямо число значи по-трудна дъска.
--      Невалидни стойности ще бъдат закръглени до най-близката валидна.
--
-- Решаване на дадена дъска
--  > solve [дъска]
--    Дъската трябва да е зададена по гореспоменатия начин.
--    Ако не е валидна по някаква причина, ще върне [[]], ако е валидна ще я реши
--

import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)

type Coord = (Int, Int)
type Board = [Int]

test_board :: [Board]
test_board = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

emptyBoard :: Board
emptyBoard = [0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0]

--ПОМОЩНИ ФУНКЦИИ

--От списък от списъци прави списък
flatten :: [Board] -> Board
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--Връща списък в списък от списъци
unflatten :: Board -> [Board]
unflatten [] = []
unflatten b = [(take 9 b)] ++ unflatten (drop 9 b)

--Връща координатите на елемент на даден номер
getElementAtNum :: Int -> Coord
getElementAtNum i = ((i - 9 * (quot i 9)), (quot i 9))

--Връща номера на елемента на координати
getElementAtCoord :: Coord -> Int
getElementAtCoord (x, y) = x + y * 9

--Взима колоната на i-тия елемент
columnAt :: Board -> Int -> [Int]
columnAt b i = helperColumnAt (getElementAtNum i) b
  where helperColumnAt (x, _) b = map (\y -> b !! getElementAtCoord (x, y)) [0..8]

--Взима реда на i-тия елемент
rowAt :: Board -> Int -> [Int]
rowAt b i = helperRowAt (getElementAtNum i) b
  where helperRowAt (_, y) b = map (\x -> b !! getElementAtCoord (x, y)) [0..8]

--Взима i-ти квадрант (3х3) отгоре надолу и отляво надясно
quadAt :: Board -> Int -> [Int]
quadAt b i = helperQuadAt (getElementAtNum i) b
  where helperQuadAt (x, y) b = [b !! getElementAtCoord (xx + (3 * (quot x 3)), yy + (3 * (quot y 3))) | xx <- [0..2], yy <- [0..2]]

--Премахва елемент(и) от списък
removeItemFromList :: Int -> [Int] -> [Int]
removeItemFromList _ [] = []
removeItemFromList x (l:ls) = if (x == l) then (removeItemFromList x ls) else [l] ++ (removeItemFromList x ls)

--Премахва срещанията на елементите от първия лист във втория
removeListFromList :: [Int] -> [Int] -> [Int]
romoveListFromList _ [] = []
removeListFromList [] l = l
removeListFromList (x:xs) l = removeListFromList xs (removeItemFromList x l)

--Намира дали елемент присъства в списък
search :: [Int] -> Int -> Bool
search [] _       = False
search (x:xs) elm = if x == elm then True else search xs elm

--Проверява дали елементите на списък са уникални
uniqueElems :: [Int] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = if (search xs x) then False else uniqueElems xs

--Проверява правилата за някоя дъска
checkRules :: Board -> Bool
checkRules b = helperCheckRules b 0
  where helperCheckRules b i | i == 9  = True
                             | ((uniqueElems (removeItemFromList 0 (rowAt b (9 * i)))) &&
                             (uniqueElems (removeItemFromList 0 (columnAt b i))) &&
                             (uniqueElems (removeItemFromList 0 (quadAt b (3 * i))))) = helperCheckRules b (i+1)
                             | otherwise = False

--Проверява дали всеки елемент има възможно решение
validate :: [Board] -> Bool
validate b = helperValidate (flatten b) 0
  where helperValidate b i  | i == 81                    = True
                            | possibleElements b i == [] = False
                            | otherwise                  = helperValidate b (i + 1)

--Проверка за валидност на дадена дъска
checkValidity :: [Board] -> Bool
checkValidity b = (checkRules (flatten b)) && (validate b)

--КРАЙ НА ПОМОЩНИТЕ ФУНКЦИИ
--ФУНКЦИИ ЗА РЕШАВАНЕ

--Връща възможните елементи за дадена клетка, ако тя е вече попълнена връща стойността й
possibleElements :: Board -> Int -> [Int]
possibleElements b i
    | i > length b  = []
    | (b !! i) == 0 = removeListFromList (columnAt b i ++ rowAt b i ++ quadAt b i) [1..9]
    | otherwise     = [b !! i]


--Променя стойността на елемент i в даден списък
setElementAt :: Int -> [Int] -> Int -> [Int]
setElementAt n xs newElement = take n xs ++ [newElement] ++ drop (n + 1) xs

--Връща следващия празен елемент
nextEmpty :: Board -> Int -> Int
nextEmpty _ 80 = 80
nextEmpty b i
    | b !! i == 0 = i
    | otherwise   = nextEmpty b (i + 1)

--Първоначалната дъска -> Докъде сме стигнали -> Донякъде попълнена дъска -> Правилна дъска
genSolution :: Board -> Int -> [Int] -> Board
genSolution _ _ [] = []
genSolution b 80 (x:xs)
    | xs == []  = setElementAt 80 b x
    | otherwise = []
genSolution b i (x:xs)
    | genNext == [] = genSolution b i xs
    | otherwise        = genNext
      where genNext = (genSolution newBord nextE (possibleElements newBord nextE))
              where newBord = setElementAt i b x
                    nextE = nextEmpty b (i + 1)

--КРАЙ НА ФУНКЦИИТЕ ЗА РЕШАВАНЕ

--Проверява дали судоку е валидно и ако е го решава
solve :: [Board] -> [Board]
solve b = if (checkRules flatB) && (validate b) then unflatten (genSolution flatB 0 (possibleElements flatB 0)) else [[]]
  where flatB = flatten b


--ФУНКЦИИ ЗА ГЕНЕРИРАНЕ

--Взима случайно число в интервала [i, j]
getRandom :: Int -> Int -> Int
getRandom i j = unsafePerformIO (getStdRandom (randomR (i, j)))

--Връща случаен елемент от списък
getRandomElem :: [Int] -> Int
getRandomElem [] = 0
getRandomElem l  = l !! (getRandom 0 ((length l) - 1))

--Генерира валидна дъска с елементи до диагонала
genRandomBoard :: Board -> Board
genRandomBoard b = helperGenRandB b 0
  where helperGenRandB b i | i > 80     = b
                           | otherwise  =  helperGenRandB (setElementAt i b (getRandomElem (possibleElements b i))) (i + 10)

--Премахва случайни различни от нула елементи от списък
remRandElms :: Board -> Int -> Board
remRandElms b i = if (i < 69) then helperRandElms b i [0..80] else helperRandElms b 69 [0..80]
  where helperRandElms b i l
            | i == 0      = b
            | otherwise   = helperRandElms (setElementAt getRand b 0) (i - 1) (removeItemFromList getRand l)
              where getRand = getRandomElem l

--Генерира случайна дъска за игра на Судоку
genBoard :: Int -> [Board]
genBoard hardness = unflatten (remRandElms (flatten (solve (unflatten (genRandomBoard emptyBoard)))) hardness)
