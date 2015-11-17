module CDMA

import StdEnv, StdLib

:: Code :== [Int]
:: Message :== [Int]
:: Bit :== Bool
:: BitSequence :== [Bit]

Start = (and (flatten alltests), alltests)
  where
    alltests = [ test_encodeFromBit
               , test_encodeFromBitSequence
               , test_interfere
               , test_calculateInterference
               , test_decodeToBit
               , test_splitMessage
               , test_decodeToBitSequence
               , test_sendData
               , test_receiveData
               ]

encodeFromBit :: Code Bit -> Code
encodeFromBit [] _ = []
encodeFromBit c b
	| b == True = c
	| b == False = map ((*)(~1)) c

test_encodeFromBit :: [Bool]
test_encodeFromBit =
  [ encodeFromBit [] True           == []
  , encodeFromBit [] False          == []
  , encodeFromBit [1,1,1] True      == [1,1,1]
  , encodeFromBit [1,-1] False      == [-1,1]
  , encodeFromBit [1,-1,-1,1] False == [-1,1,1,-1]
  , encodeFromBit [1,-1,-1,1] True  == [1,-1,-1,1]
  ]

encodeFromBitSequence :: Code BitSequence -> Message
encodeFromBitSequence _ [] = []
encodeFromBitSequence c bs = (encodeFromBit c (hd bs))++(encodeFromBitSequence c (drop 1 bs))

test_encodeFromBitSequence :: [Bool]
test_encodeFromBitSequence =
  [ encodeFromBitSequence [1,1]     []                            == []
  , encodeFromBitSequence [1,-1]    [True,False,False,True,False] == [1,-1,-1,1,-1,1,1,-1,-1,1]
  , encodeFromBitSequence [1,-1,1]  [False,True,True]             == [-1,1,-1,1,-1,1,1,-1,1]
  , encodeFromBitSequence [1,1,1,1] [False,True,False,False,True] == [-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1]
  ]

interfere :: Message Message -> Message
interfere [] [] = []
interfere [] m = m
interfere m [] = m
interfere m1 m2 = [(hd m1)+(hd m2)]++(interfere (drop 1 m1) (drop 1 m2))

test_interfere :: [Bool]
test_interfere =
  [ interfere []                              []                              == []
  , interfere [1,1,-1,-1,1,1,1,1]             [-1,1,1,-1,1,-1,-1,1]           == [0,2,0,-2,2,0,0,2]
  , interfere [-1,1,-1,1,1,-1,1,-1,-1,1,-1,1] [1,-1,-1,1,1,-1,-1,1,-1,1,1,-1] == [0,0,-2,2,2,-2,0,0,-2,2,0,0]
  ]

calculateInterference :: [Message] -> Message
calculateInterference [] = []
calculateInterference m = foldl interfere [] m

test_calculateInterference :: [Bool]
test_calculateInterference =
  [ calculateInterference []                                              == []
  , calculateInterference [ [] , [] , [] ]                                == []
  , calculateInterference [ [1,1,-1,-1,1,1,1,1] , [-1,1,1,-1,1,-1,-1,1] ] == [0,2,0,-2,2,0,0,2]
  , calculateInterference [ [1,1,1,1,-1,-1,-1,-1,1,1,1,1] , [-1,-1,1,1,1,1,-1,-1,1,1,-1,-1] , [1,-1,1,-1,1,-1,1,-1,-1,1,-1,1] ] == [1,-1,3,1,1,-1,-1,-3,1,3,-1,1]
  ]

scalarMul :: Code Code -> Int
scalarMul [] [] = 1
scalarMul c1 c2 = (hd c1)*(hd c2) + (scalarMul (drop 1 c1) (drop 1 c2))

decodeToBit :: Code Code -> Bit
decodeToBit c1 c2 = (scalarMul c1 c2) > 0

test_decodeToBit :: [Bool]
test_decodeToBit =
  [ decodeToBit [1,-1,1,-1] [1,-1,3,1]   == True
  , decodeToBit [1,1,-1,-1] [1,-1,3,1]   == False
  , decodeToBit [1,1,-1,-1] [1,-1,-1,-3] == True
  ]

splitMessage :: Int Message -> [Code]
splitMessage _ [] = []
splitMessage n m = [(take n m):(splitMessage n (drop n m))]

test_splitMessage :: [Bool]
test_splitMessage =
  [ splitMessage 5 []                             == []
  , splitMessage 1 [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [[1],[-1],[3],[1],[1],[-1],[-1],[-3],[1],[3],[-1],[1]]
  , splitMessage 4 [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [[1,-1,3,1],[1,-1,-1,-3],[1,3,-1,1]]
  , splitMessage 2 [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [[1,-1],[3,1],[1,-1],[-1,-3],[1,3],[-1,1]]
  , splitMessage 3 [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [[1,-1,3],[1,1,-1],[-1,-3,1],[3,-1,1]]
  ]

decodeToBitSequence :: Code Message -> BitSequence
decodeToBitSequence _ [] = []
decodeToBitSequence c m = map (decodeToBit c) (splitMessage (length c) m)

test_decodeToBitSequence :: [Bool]
test_decodeToBitSequence =
  [ decodeToBitSequence [1,1]       []                             == []
  , decodeToBitSequence [1,1,1,1]   [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [True,False,True]
  , decodeToBitSequence [1,-1,1,-1] [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [True,True,False]
  , decodeToBitSequence [1,1,-1,-1] [1,-1,3,1,1,-1,-1,-3,1,3,-1,1] == [False,True,True]
  ]

calculateMessages :: [(Code, BitSequence)] -> [Message]
calculateMessages [] = []
calculateMessages data = [encodeFromBitSequence (fst (hd data)) (snd (hd data))]++(calculateMessages (drop 1 data))

sendData :: [(Code, BitSequence)] -> Message
sendData [] = []
sendData data = calculateInterference (calculateMessages data)

test_sendData :: [Bool]
test_sendData =
  [ sendData []                                   == []
  , sendData [sender1]                            == [1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1]
  , sendData [sender1, sender2]                   == [0,2,0,2,0,-2,0,-2,0,2,0,2,0,-2,0,-2]
  , sendData [sender3, sender2, sender1]          == [-1,1,1,3,1,-1,-1,-3,1,3,-1,1,-1,-3,1,-1]
  , sendData [sender1, sender4, sender3, sender2] == [-2,2,2,2,0,0,0,-4,0,4,0,0,0,-4,0,0]
  ]
  where
    sender1 = ([1, 1, 1, 1], [True,False,True,False])
    sender2 = ([1,-1, 1,-1], [False,True,False,True])
    sender3 = ([1, 1,-1,-1], [False,True,True,False])
    sender4 = ([1,-1,-1, 1], [False,False,False,True])

makeBS :: [Code] Message -> [BitSequence]
makeBS [] _ = []
makeBS c m = [decodeToBitSequence (hd c) m]++(makeBS (drop 1 c) m)

receiveData :: [Code] Message -> [(Code, BitSequence)]
receiveData [] _ = []
receiveData c m = zip c (makeBS c m)

test_receiveData :: [Bool]
test_receiveData =
  [ receiveData [] [-2,2,2,2,0,0,0,-4,0,4,0,0,0,-4,0,0]          == []
  , receiveData [[1,-1,1,-1],[1,1,-1,-1]] []                     == [ ([1,-1,1,-1], []) , ([1,1,-1,-1], []) ]
  , receiveData [[1,1,1,1]] [-2,2,2,2,0,0,0,-4,0,4,0,0,0,-4,0,0] == [([1,1,1,1],[True,False,True,False])]
  , receiveData [[1,-1,1,-1],[1,1,-1,-1],[1,-1,-1,1]] [-2,2,2,2,0,0,0,-4,0,4,0,0,0,-4,0,0]
      == [ ([1,-1,1,-1], [False,True,False,True]) , ([1,1,-1,-1], [False,True,True,False]) , ([1,-1,-1,1], [False,False,False,True]) ]
  ]
