import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M

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
  memLeft (T {tVec = v, tIx = 0}) = (T {tVec = v, tIx = (length v) - 1})
  memLeft (T {tVec = v, tIx = i}) = (T {tVec = v, tIx = i - 1})
  memRight (T {tVec = v, tIx = i})
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
  , memRight  t                 ==  t { tIx = 1 }
  , memRight  t { tIx = 3 }     ==  t { tIx = 0 }
  , memLeft t { tIx = 3 }       ==  t { tIx = 2 }
  , memLeft t                   ==  t { tIx = 3 }
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
  
main = do
  print test_BFMem_Tape
  print test_parseProgram
  print test_matchingBracket
  getLine
  
  
  
  
  