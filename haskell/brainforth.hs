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
  , memRight  t                  ==  t { tIx = 1 }
  , memRight  t { tIx = 3 }      ==  t { tIx = 0 }
  , memLeft t { tIx = 3 }      ==  t { tIx = 2 }
  , memLeft t                  ==  t { tIx = 3 }
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

parseProgram :: String -> BFEnv
parseProgram str
  | str == "" = M.empty
  
  
test_parseProgram =
  [ parseProgram "+-<>[],."    == M.fromList [(sq0, V.fromList [Inc, Dec, MemLeft, MemRight, BrktOpen, BrktClose, In, Out])]
  , parseProgram ":A-;A+"      == M.fromList [(sq0, V.fromList [SeqId 'A', Inc]), ('A', V.fromList [Dec])]
  , parseProgram ":A-;:B+;AB+" == M.fromList [(sq0, V.fromList [SeqId 'A', SeqId 'B', Inc]), ('A', V.fromList [Dec]), ('B', V.fromList [Inc])]
  ]  

main = do
  print test_BFMem_Tape
  print test_parseProgram
  
  
  
  
  