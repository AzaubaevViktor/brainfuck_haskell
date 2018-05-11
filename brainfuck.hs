import Data.Char

promptLine prompt = do
    putStr prompt
    getLine

main = do
  cod <- promptLine "Enter brainfuck code:\n"
  do
    _ <- getLine
    input <- promptLine "Enter input:\n"
    do
      putStrLn "Run programm"
      let state = newState cod input
      print $ inData state
      -- print $ mem state
      print $ inData state
      print $ code state
      print $ cp state
      print $ mp state
      putStrLn "==========================="
      let afterState = interpreter state
      putStrLn $ out $ afterState
      putStrLn "==========================="
      -- print $ mem afterState
      print $ inData afterState
      print $ code afterState
      print $ cp afterState
      print $ mp afterState
      

data State = State {
  mem :: [Int],
  cp :: Int,
  mp :: Int, 
  code :: String,
  out :: String,
  inData :: String
}

_newMem :: [Int] -> Int -> [Int]
_newMem a 0 = a
_newMem a n = _newMem (0:a) $ dec n

newMem = _newMem []

filterCode = filter ((flip elem) "+-<>[].,") 

newState cod input = (State (newMem 30000) 0 0 (filterCode cod) "" input)

curCode :: State -> Char
curCode state = (code state) !! cp state

changeArr :: (a -> a) -> [a] -> Int -> [a]
changeArr f (x:arr) 0 = (f x) : arr
changeArr f (x:arr) n = x : (changeArr f arr $ n - 1)

changeMem :: (Int -> Int) -> State -> State
changeMem f state = (State 
  (changeArr f (mem state) $ mp state)
  (cp state)
  (mp state)
  (code state)
  (out state)
  (inData state))
  
curMem :: State -> Int
curMem state = (mem state) !! mp state

changeCP :: (Int -> Int) -> State -> State
changeCP f state = (State 
  (mem state)
  ((f . cp) state)
  (mp state)
  (code state)
  (out state)
  (inData state))

changeMP :: (Int -> Int) -> State -> State
changeMP f state = (State 
  (mem state)
  (cp state)
  ((f . mp) state)
  (code state)
  (out state)
  (inData state))
  
appendOut :: State -> Char -> State
appendOut state c = (State 
  (mem state)
  (cp state)
  (mp state)
  (code state)
  ((out state) ++ [c])
  (inData state))

update :: Int -> Int -> Int
update x y = x

getFromIn :: State -> State
getFromIn state = (State 
  (mem $ changeMem (update $ ord $ head $ inData state) state)
  (cp state)
  (mp state)
  (code state)
  (out state)
  (tail $ inData state))
  
getCloseBraceCP :: String -> Int -> Int -> Int
getCloseBraceCP cod curInd 0 = case (cod !! curInd) of
  ']' -> curInd
  '[' -> getCloseBraceCP cod (inc curInd) 1
  otherwise -> getCloseBraceCP cod (inc curInd) 0
getCloseBraceCP cod curInd n = case (cod !! curInd) of
  ']' -> getCloseBraceCP cod (inc curInd) (dec n)
  '[' -> getCloseBraceCP cod (inc curInd) (inc n)
  otherwise -> getCloseBraceCP cod (inc curInd) n
  
getOpenBraceCP :: String -> Int -> Int -> Int
getOpenBraceCP cod curInd 0 = case (cod !! curInd) of
  '[' -> curInd
  ']' -> getOpenBraceCP cod (dec curInd) 1
  otherwise -> getOpenBraceCP cod (dec curInd) 0
getOpenBraceCP cod curInd n = case (cod !! curInd) of
  '[' -> getOpenBraceCP cod (dec curInd) (dec n)
  ']' -> getOpenBraceCP cod (dec curInd) (inc n)
  otherwise -> getOpenBraceCP cod (dec curInd) n

_closeBraceCp state = getCloseBraceCP (code state) (inc $ cp state) 0
-- Дошли до закрываеющей скобки, после этого будет +1
closeBraceCp state = (State 
  (mem state)
  (_closeBraceCp state)
  (mp state)
  (code state)
  (out state)
  (inData state))
  
_openBraceCp state = getOpenBraceCP (code state) (dec $ cp state) 0
-- Перепрыгнули на открывающуюся скобку и надо проверить обязательно условие
-- Поэтому ещё на шаг назад, ибо будет +1
openBraceCp state = (State
  (mem state)
  ((dec . _openBraceCp) state)
  (mp state)
  (code state)
  (out state)
  (inData state))
  
inc = (+ 1)
dec = (+ (-1))
incMem x = if (x == 255) then 0 else x + 1
decMem x = if (x == 0) then 255 else x - 1

step :: State -> State
step state = changeCP (+ 1) (case (curCode state) of
  '+' -> changeMem (incMem) state
  '-' -> changeMem (decMem) state
  '>' -> changeMP (inc) state
  '<' -> changeMP (dec) state
  '[' -> if ((curMem state) == 0)
          then closeBraceCp state
          else state
  ']' -> openBraceCp state
  '.' -> appendOut state $ chr $ curMem state
  ',' -> getFromIn state)

interpreter :: State -> State
interpreter state = if ((cp state) >= (length $ code state))
  then state
  else interpreter $ step state
