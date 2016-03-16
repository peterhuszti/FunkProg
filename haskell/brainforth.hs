import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace(trace)

data Tape = T
  { tVec :: Vector Int
  , tIx  :: Int
  } deriving (Show, Eq)

newTape :: Int -> Tape
newTape n = (T {tVec = V.generate n (*0), tIx = 0})
  
class BFMem m where
  incVal    :: m -> m
  decVal    :: m -> m
  isNull    :: m -> Bool
  getVal    :: m -> Int
  putVal    :: m -> Int -> m
  memLeft   :: m -> m
  memRight  :: m -> m

instance BFMem Tape where
  incVal (T {tVec = v, tIx = i}) = (T {tVec = v V.// [(i, (v V.! i) + 1)], tIx = i})
  decVal (T {tVec = v, tIx = i}) = (T {tVec = v V.// [(i, (v V.! i) - 1)], tIx = i})
  isNull (T {tVec = v, tIx = 0}) = True
  isNull (T {tVec = v, tIx = _}) = False
  getVal (T {tVec = v, tIx = i}) = v V.! i
  putVal (T {tVec = v, tIx = i}) x = (T {tVec = v V.// [(i, x)], tIx = i})
  memRight (T {tVec = v, tIx = 0}) = (T {tVec = v, tIx = (length v) - 1})
  memRight (T {tVec = v, tIx = i}) = (T {tVec = v, tIx = i - 1})
  memLeft (T {tVec = v, tIx = i})
    | i == (length v) - 1 = (T {tVec = v, tIx = 0})
    | otherwise = (T {tVec = v, tIx = i + 1})
  
test_BFMem_Tape =
  [ incVal   t                  ==  t { tVec = V.fromList [ 1, 1, 2, 3] }
  , decVal   t                  ==  t { tVec = V.fromList [-1, 1, 2, 3] }
  , isNull   t                  ==  True
  , isNull   t { tIx = 1 }      ==  False
  , getVal   t                  ==  0
  , getVal   t { tIx = 3 }      ==  3
  , putVal   t             451  ==  t { tVec = V.fromList [451, 1, 2, 3] }
  , putVal   t { tIx = 3 } 451  ==  T { tVec = V.fromList [0, 1, 2, 451], tIx = 3 }
  , memLeft  t                  ==  t { tIx = 1 }
  , memLeft  t { tIx = 3 }      ==  t { tIx = 0 }
  , memRight t { tIx = 3 }      ==  t { tIx = 2 }
  , memRight t                  ==  t { tIx = 3 }
  ]
  where t = T { tVec = V.fromList [0, 1, 2, 3], tIx = 0 }
  
data BFSymbol
  = Inc | Dec | MemLeft | MemRight | BrktOpen | BrktClose | In | Out
  | StartSeq | EndSeq | SeqId Char
  deriving (Show, Eq) 
  
type BFSequence = Vector BFSymbol
type BFEnv = Map Char BFSequence
  
sq0 :: Char
sq0 = '*'

convertCharToBFSymbol :: Char -> BFSymbol
convertCharToBFSymbol c
  | c == '+' = Inc
  | c == '-' = Dec
  | c == '>' = MemRight
  | c == '<' = MemLeft
  | c == ',' = In
  | c == '.' = Out
  | c == '[' = BrktOpen
  | c == ']' = BrktClose
  | c == ':' = StartSeq
  | c == ';' = EndSeq
  | otherwise = SeqId c

convertStringToTokens :: String -> BFSequence
convertStringToTokens str = V.fromList (map convertCharToBFSymbol str)
  
dummy :: BFSequence -> BFSequence -> BFSequence
dummy a b = a
  
parseProgram :: String -> BFEnv
parseProgram str
  | str !! 0 == ':' =  M.unionWith (dummy) (M.fromList [(str !! 1, convertStringToTokens (take (findSemiColon (drop 2 str) 0) (drop 2 str)))]) (parseProgram (drop (findSemiColon (drop 2 str) 3) str))
  | otherwise = M.fromList [(sq0, convertStringToTokens str)]

findSemiColon :: String -> Int -> Int
findSemiColon str n
  | str !! 0 == ';' = n
  | otherwise = findSemiColon (drop 1 str) n+1
  
test_parseProgram =
  [ parseProgram "+-<>[],."    == M.fromList [(sq0, V.fromList [Inc, Dec, MemLeft, MemRight, BrktOpen, BrktClose, In, Out])]
  , parseProgram ":A-;A+"      == M.fromList [(sq0, V.fromList [SeqId 'A', Inc]), ('A', V.fromList [Dec])]
  , parseProgram ":A-;:B+;AB+" == M.fromList [(sq0, V.fromList [SeqId 'A', SeqId 'B', Inc]), ('A', V.fromList [Dec]), ('B', V.fromList [Inc])]
  ]  

matchingBracket :: BFSequence -> Int -> Int
matchingBracket seq n
  | seq V.! n == BrktOpen = findCloser (drop (n+1) (V.toList(seq))) 0 (n+1)
  | seq V.! n == BrktClose = findOpener (take n (V.toList(seq))) 0 (n-1)

findCloser :: [BFSymbol] -> Int -> Int -> Int
findCloser a n i
  | head a == BrktClose && n == 0 = i
  | head a == BrktClose = findCloser (drop 1 a) (n-1) (i+1)
  | head a == BrktOpen = findCloser (drop 1 a) (n+1) (i+1)
  | otherwise = findCloser (drop 1 a) n (i+1)

findOpener :: [BFSymbol] -> Int -> Int -> Int
findOpener a n i
  | last a == BrktOpen && n == 0 = i
  | last a == BrktOpen = findOpener (init a) (n-1) (i-1)
  | last a == BrktClose = findOpener (init a) (n+1) (i-1)
  | otherwise = findOpener (init a) n (i-1)
  
 
test_matchingBracket = testBrkt sq1 pairs1 ++ testBrkt sq2 pairs2
  where
    testBrkt sq pairs = map (\(s, e) -> matchingBracket (mkSq sq) s == e) pairs
    mkSq   = V.fromList . map (\c -> case c of '(' -> BrktOpen; ')' -> BrktClose; _ -> Inc)
    sq1    = "(a)(b)"
    pairs1 = [(0, 2), (3, 5)]
    sq2    = "((())()())"
    pairs2 = zip [0..9] [9, 4, 3, 2, 1, 6, 5, 8, 7, 0]
  
type Stack = [(Int, Char)]
  
data BFState = S
  { sCallStk :: Stack
  , sMem     :: Tape
  , sIn      :: [Int]
  , sOut     :: [Int]
  } deriving (Show, Eq)
  
pop :: BFState -> BFState
pop S {sCallStk = stack, sMem = m, sIn = i, sOut = o} = S {sCallStk = tail stack, sMem = m, sIn = i, sOut = o}

push :: Char -> BFState -> BFState
push x S {sCallStk = stack, sMem = m, sIn = i, sOut = o} = S {sCallStk = ([(0,x)] ++ stack), sMem = m, sIn = i, sOut = o}
  
getCommand :: BFEnv -> Char -> Int -> BFSymbol
getCommand env seqID index = (fromJust (M.lookup seqID env)) V.! index

stepTopToN :: Int -> BFState -> BFState
stepTopToN n S {sCallStk = stack@[(index,seqID)], sMem = m, sIn = i, sOut = o} = S {sCallStk = [(n,seqID)], sMem = m, sIn = i, sOut = o}

isSeqId :: BFSymbol -> Bool
isSeqId s
  | s == Inc || s == Dec || s == MemLeft || s == MemRight || s == BrktOpen || s == BrktClose || s == In || s == Out || s == StartSeq || s == EndSeq = False
  | otherwise = True

getSeqId :: BFSymbol -> Char
getSeqId (SeqId c) = c

stepTop :: BFState -> BFState
stepTop S {sCallStk = (index,seqID):xs, sMem = m, sIn = i, sOut = o} = S {sCallStk = ((index+1),seqID):xs, sMem = m, sIn = i, sOut = o}

memControl :: BFSymbol -> BFState -> BFState
memControl command (S s m i o)
    | command == Inc = ( S s (incVal m) i o)
    | command == Dec = ( S s (decVal m) i o)
    | command == MemLeft = ( S s (memLeft m) i o)
    | command == MemRight = ( S s (memRight m) i o)
    | command == In = ( S s (putVal m (head i)) (tail i) o)
    | command == Out = ( S s m i ((getVal m):o))
    | otherwise =  (S s m i o)

isLastCommand :: BFSequence -> Int -> Bool
isLastCommand s index
  | (length s) == index = True
  | otherwise = False

getNewState :: BFEnv -> BFState -> BFState
getNewState env oldState@(S {sCallStk = ((index,seqID):xs), sMem = tape, sIn = input, sOut = output})
    | (isLastCommand (fromJust (M.lookup seqID env)) index) = pop oldState
    | command == BrktOpen && (getVal tape) == 0 = stepTopToN ((matchingBracket (fromJust (M.lookup seqID env)) index)+1) oldState
    | command == BrktOpen = (S [((index+1),seqID)] tape input output)
    | command == BrktClose = stepTopToN (matchingBracket (fromJust (M.lookup seqID env)) index) oldState
    | isSeqId command = push (getSeqId command) (stepTop oldState)
    | otherwise = stepTop (memControl command oldState)
    where
        command = getCommand env seqID index

step :: ReaderT BFEnv (State BFState) ()
step = ReaderT ( \e -> state (\oldState -> ((),getNewState e oldState)))
  
test_step =
  [ exec env1 st1{sCallStk = [(0, sq0)]} == st1{sCallStk = [(1, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(1, sq0)]} == st1{sCallStk = [(2, sq0)], sMem = memRight $ newTape 32}
  , exec env1 st1{sCallStk = [(2, sq0)]} == st1{sCallStk = [(5, sq0)]}
  , exec env1 st1{sCallStk = [(2, sq0)], sMem = incVal $ newTape 32} == st1{sCallStk = [(3, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(4, sq0)]} == st1{sCallStk = [(2, sq0)]}
  , exec env1 st1{sCallStk = [(5, sq0)]} == st1{sCallStk = [(6, sq0)], sMem = putVal (newTape 32) 43, sIn = []}
  , exec env1 st1{sCallStk = [(6, sq0)]} == st1{sCallStk = [(7, sq0)], sOut = [0]}

  , exec env2 st2{sCallStk = [(1, sq0)]} == st2{sCallStk = [(0, 'A'), (2, sq0)]}
  , exec env2 st2{sCallStk = [(0, 'A'), (2, sq0)]} == st2{sCallStk = [(1, 'A'), (2, sq0)], sMem = incVal $ newTape 32}
  , exec env2 st2{sCallStk = [(1, 'A'), (2, sq0)]} == st2{sCallStk = [(2, sq0)]}
  ]
  where
    exec env st = execState (runReaderT step env) st

    env1 = M.fromList [(sq0, V.fromList [Inc, MemRight, BrktOpen, Inc, BrktClose, In, Out])]
    st1  = S {sCallStk = [], sMem = newTape 32, sIn = [43], sOut = []}

    env2 = M.fromList [(sq0, V.fromList [Dec, SeqId 'A']), ('A', V.fromList [Inc])]
    st2  = S {sCallStk = [], sMem = newTape 32, sIn = [], sOut = []}

oneStep :: BFState -> BFEnv -> [Int] -> BFState
oneStep state env input = execState (runReaderT step env) state

runProgram :: String -> [Int] -> BFState
runProgram program input = oneStep startState (parseProgram program) input
  where
    startState = S {sCallStk = [(0, sq0)], sMem = newTape 32, sIn = input, sOut = []}

-- test_runProgram =
  -- [ runProgram sqSimple    []           == [3, 4, 5, 4, 3]
  -- , runProgram sqLoop      []           == [4, 3, 2, 1, 0]
  -- , runProgram sqInput     [69, 418]    == [69, 420]
  -- , runProgram sqMovePtr   [1, 2, 3]    == [3, 2, 1]
  -- , runProgram sqSimpleSq  []           == [0, 2]
  -- , runProgram sqAddThree  [1, 10, 100] == [111]
  -- ]
  -- where
    -- sqSimple    = "+++.+.+.-.-."
    -- sqLoop      = "++++[.-]."
    -- sqInput     = ",.,++."
    -- sqMovePtr   = ",>,>,.<.<."
    -- sqSimpleSq  = ":A++;.A."
    -- sqAddThree  = ":A>[-<+>]<;,>,>,<A<A."

main = do
  print test_BFMem_Tape
  print test_parseProgram
  print test_matchingBracket
  print test_matchingBracket
  print test_step
 -- print test_runProgram
  getLine
  
  
  
  
  