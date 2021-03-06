module Gomoku

import StdEnv, StdLib, StdBool

:: Direction = Horizontal | Vertical | LeftDiagonal | RightDiagonal
:: Player = X | O
:: Position :== (Int, Int)
:: Mark :== (Player, Position)
:: Board :== [Mark]
:: GameState :== (Player, Board)

// Szukseges, hogy az (==) fuggvenyt hasznalhassuk Player tipusu ertekekre.
instance == Player where
  (==) X X = True
  (==) O O = True
  (==) _ _ = False

// Szukseges, hogy az toString fuggvenyt hasznalhassuk Player tipusu ertekekre.
instance toString Player where
  toString X = "X"
  toString O = "O"
  
instance == Direction where
	(==) Horizontal Horizontal = True
	(==) Vertical Vertical = True
	(==) LeftDiagonal LeftDiagonal = True
	(==) RightDiagonal RightDiagonal = True
	(==) _ _ = False

Start = (and (flatten allTests), allTests)
  where
    allTests =
      [ test_nextPlayer
      , test_emptyGame
      , test_nextStep
      , test_checkNInARow
      , test_lineFilter
      , test_project
      , test_checkBoardForN
      , test_playGame
      , test_areValidSteps
      , test_startGame
      ]

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X 

test_nextPlayer :: [Bool]
test_nextPlayer =
  [ nextPlayer X == O
  , nextPlayer O == X
  ]

emptyGame :: GameState
emptyGame = (X,[])

test_emptyGame :: [Bool]
test_emptyGame =
  [ emptyGame == (X, [])
  ]

nextStep :: Position GameState -> GameState
nextStep a (b,c) = (nextPlayer (b),([(b,(a)):c]))

test_nextStep :: [Bool]
test_nextStep =
  [ nextStep (1,1) emptyGame == (O,[(X,(1,1))])
  , foldr (\x y -> x == (O,(1,2)) || y) False (snd (nextStep (1,2) (O,[(X,(2,1))])))
  ]

checkNInARow :: Int [Int] -> Bool
checkNInARow n list
	| length(removeDup(list)) < n = False
	| (hd (sort(list))) + (n-1) == (removeDup(sort(list)))!!(n-1) = True
	| otherwise = checkNInARow n (drop 1 (sort(list)))

test_checkNInARow :: [Bool]
test_checkNInARow =
  [ checkNInARow 3 [3,1,~2,8,2]        == True
  , checkNInARow 4 [3,1,~2,8,2,99,~42] == False
  , checkNInARow 1 [3]                 == True
  , checkNInARow 5 []                  == False
  , checkNInARow 4 [0,1,42,~2,~1,99,8] == True
  , checkNInARow 3 [3,1,~2,1,2,8,2]    == True
  , checkNInARow 4 [3,1,~2,1,2,8,2]    == False
  ]

lineFilter :: Mark Direction -> (Mark -> Bool)
lineFilter m dir = lf m dir

lf :: Mark Direction Mark -> Bool
lf (p1,(x1,y1)) dir (p2,(x2,y2)) 
	| dir == Horizontal = (x1 == x2 && p1 == p2)
	| dir == Vertical = (y1 == y2 && p1 == p2)
	| dir == LeftDiagonal = (y2-y1 == x2-x1 && p1 == p2)
	| dir == RightDiagonal = (y1-y2 == x2-x1 && p1 == p2)
	| otherwise = False

test_lineFilter :: [Bool]
test_lineFilter =
  [ lineFilter (X,(3,3)) Horizontal (X,(3,3))     == True
  , lineFilter (X,(3,3)) Horizontal (X,(3,5))     == True
  , lineFilter (X,(3,3)) Horizontal (O,(0,0))     == False
  , lineFilter (X,(3,3)) Horizontal (X,(0,0))     == False
  , lineFilter (O,(0,0)) Vertical (O,(0,0))       == True
  , lineFilter (O,(0,0)) Vertical (X,(0,0))       == False
  , lineFilter (O,(0,0)) Vertical (O,(5,0))       == True
  , lineFilter (O,(3,3)) Vertical (O,(2,2))       == False
  , lineFilter (O,(0,0)) Vertical (O,(0,5))       == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(0,0))   == True
  , lineFilter (X,(0,0)) LeftDiagonal (O,(0,0))   == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(3,3))   == True
  , lineFilter (X,(0,0)) LeftDiagonal (O,(2,2))   == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(3,0))   == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(0,0))  == True
  , lineFilter (X,(0,0)) RightDiagonal (O,(0,0))  == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(~3,3)) == True
  , lineFilter (X,(0,0)) RightDiagonal (O,(~3,3)) == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(3,3))  == False
  ]

project :: Direction -> (Mark -> Int)
project dir = proj dir

proj :: Direction Mark -> Int
proj dir (p,(x,y))
	| dir == Horizontal = y
	| otherwise = x

test_project :: [Bool]
test_project =
  [ project Horizontal (X,(1,2))    == 2
  , project Vertical (O,(1,2))      == 1
  , project LeftDiagonal (X,(1,2))  == 1
  , project RightDiagonal (O,(1,2)) == 1
  ]

checkBoardForN :: Int Mark Board Direction -> Maybe Player
checkBoardForN n (p,(x,y)) list dir
	| (checkNInARow n (map(project dir)((filter(lineFilter (p,(x,y)) dir)list)++[(p,(x,y))])) == True) = Just p
	| otherwise	= Nothing

test_checkBoardForN :: [Bool]
test_checkBoardForN =
  [ checkBoardForN 5 (O,(0,5)) [(X,(10,10)),(O,(0,4)),(X,(4,4)),(O,(0,3)),(X,(3,3)),(O,(0,2)),(X,(2,2)),(O,(0,1)),(X,(1,1))] Horizontal == Just O
  , checkBoardForN 3 (X,(0,0)) [(O,(0,2)),(X,(-1,-1)),(O,(0,1)),(X,(-2,-2))] LeftDiagonal  == Just X
  , checkBoardForN 3 (X,(0,0)) [(O,(0,2)),(X,(-1,-1)),(O,(0,1)),(X,(-2,-2))] RightDiagonal == Nothing
  , checkBoardForN 3 (X,(2,3)) [(O,(1,1)),(X,(1,4)),(O,(0,0)),(X,(0,5))] RightDiagonal     == Just X
  , checkBoardForN 2 (X,(0,0)) [(O,(1,1)),(X,(-1,0))] Vertical                             == Just X
  , checkBoardForN 2 (X,(1,0)) [(O,(1,1)),(X,(-1,0))] Vertical                             == Nothing
  ]

playGame :: Int [Position] -> (Maybe Player)
playGame n xs = pg [] (marks xs) n
	where
	pg :: Board Board Int -> (Maybe Player)
	pg b [] n = Nothing
	pg b [x=:(p,pos):xs] n
		| checkBoardForN n x b Horizontal == Just p = Just p
		| checkBoardForN n x b Vertical == Just p = Just p
		| checkBoardForN n x b LeftDiagonal == Just p = Just p
		| checkBoardForN n x b RightDiagonal == Just p	= Just p
		| otherwise	= pg (b ++ [x]) xs n
	marks :: [Position] -> Board
	marks [] = []
	marks xs = marks2 xs X
		where
		marks2 :: [Position] Player -> Board
		marks2 [x] p = [(p,x)]
		marks2 [x:xs] p = [(p,x)] ++ (marks2 xs (nextPlayer p))

test_playGame :: [Bool]
test_playGame =
  [ playGame 5 [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == Just O
  , playGame 3 [(~2,~2),(0,1),(~1,~1),(0,2),(0,0)]   == Just X
  , playGame 3 [(0,5),(0,0),(1,4),(1,1),(2,3)]       == Just X
  , playGame 3 [(0,5),(0,0),(1,4),(1,1)]             == Nothing
  , playGame 3 [(1,0),(1,1),(2,0),(2,2),(4,0),(3,3)] == Just O
  ]

areValidSteps :: [Position] -> Bool
areValidSteps pos = (pos == removeDup(pos))

test_areValidSteps :: [Bool]
test_areValidSteps =
  [ areValidSteps []                                     == True
  , areValidSteps [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == True
  , areValidSteps [(0,1),(3,4),(11,99),(2,3),(1,2)]      == True
  , areValidSteps [(~1,~2),(~2,~1),(10,5),(1,1),(~2,~1)] == False
  , areValidSteps [(~1,~10),(~1,~10),(1,1),(1,1)]        == False
  ]

startGame :: Int [Position] -> String
startGame n list 
	| areValidSteps list == False = "Hibas lepessorozat"
	| playGame n list == Just X = "A jatekot nyerte: X"
	| playGame n list == Just O = "A jatekot nyerte: O"
	| playGame n list == Nothing = "Nincs nyertes"

test_startGame :: [Bool]
test_startGame =
  [ startGame 5 [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == "A jatekot nyerte: O"
  , startGame 3 [(~2,~2),(0,1),(~1,~1),(0,2),(0,0)]   == "A jatekot nyerte: X"
  , startGame 3 [(0,5),(0,0),(1,4),(1,1),(2,3)]       == "A jatekot nyerte: X"
  , startGame 3 [(0,5),(0,0),(1,4),(1,1)]             == "Nincs nyertes."
  , startGame 3 [(1,0),(1,1),(2,0),(2,2),(4,0),(3,3)] == "A jatekot nyerte: O"
  , startGame 4 [(1,1),(1,1),(2,0),(2,2),(4,0),(3,3)] == "Hibas lepessorozat."
  ]
