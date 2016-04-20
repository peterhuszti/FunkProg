{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent
import Control.Parallel.Strategies

import Data.Text (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Data.Maybe
import Data.String (fromString)
import Network
import System.IO

import Data.List
import Data.Foldable (toList)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Debug.Trace (trace, traceIO)

data Matrix = M
  { mDat :: Seq Integer
  , mSize :: Int
  , mIx :: (Int, Int) -> Maybe Int
  }

newMatrix :: Int -> Matrix
newMatrix n = M
  { mDat = (Seq.replicate (n*n) 0)
  , mSize = n
  , mIx = (\(i,j) -> if i>n || j>n || i<1 || j<1 then Nothing
                     else Just (n*(i-1)+(j-1))
          )
  }
  
setMatrix :: (Int, Int) -> Integer -> Matrix -> Matrix
setMatrix (i,j) x mtx = do
    if mIx mtx (i,j) == Nothing
    then mtx
    else M { mDat = Seq.update (fromJust (mIx mtx (i,j))) x (mDat mtx), mSize = mSize mtx, mIx = mIx mtx }

getMatrix :: (Int, Int) -> Matrix -> Maybe Integer
getMatrix (i,j) mtx = do
    if mIx mtx (i,j) == Nothing
    then Nothing
    else Just(Seq.index (mDat mtx) (fromJust $ mIx mtx (i,j)))
  
test_newMatrix :: [Bool]
test_newMatrix =
  let m3 = newMatrix 3 in
  [ mDat m3 == Seq.replicate 9 0
  , mSize m3 == 3
  , mIx m3 (0, 0) == Nothing
  , mIx m3 (1, 1) == Just 0
  , mIx m3 (2, 3) == Just 5
  , mIx m3 (3, 2) == Just 7
  ]
  
test_setMatrix :: [Bool]
test_setMatrix =
  let m2 = newMatrix 2 in
  [ mDat (setMatrix (0, 0) 1 m2) == mDat m2
  , mDat (setMatrix (1, 1) 1 m2) == Seq.fromList [1, 0, 0, 0]
  , mDat (setMatrix (1, 2) 1 m2) == Seq.fromList [0, 1, 0, 0]
  ]

test_getMatrix :: [Bool]
test_getMatrix =
  let m2 = setMatrix (1, 2) 1 $ newMatrix 2 in
  [ getMatrix (0, 0) m2 == Nothing
  , getMatrix (1, 1) m2 == Just 0
  , getMatrix (1, 2) m2 == Just 1
  ]
  
addMatrix :: Matrix -> Matrix -> Maybe Matrix
addMatrix a b = do
    if mSize a /= mSize b
    then Nothing
    else Just (M {mDat = sumSeq (mDat a) (mDat b) (2*(mSize a)-1), mSize = mSize a, mIx = mIx a})

sumSeq :: Seq Integer -> Seq Integer -> Int -> Seq Integer
sumSeq s t (-1) = s
sumSeq s t i = sumSeq (Seq.update i ((Seq.index s i)+(Seq.index t i)) s) t (i-1) `using` parTraversable rseq

test_addMatrix :: [Bool]
test_addMatrix =
  let ma = newMatrix 3
      mb = setMatrix (1,2) 1 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 1 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ addMatrix ma mb
      , (mDat <$> addMatrix mb mc) == Just (Seq.fromList [2, 1, 1, 0])
      ]

mulMatrix :: Matrix -> Matrix -> Maybe Matrix
mulMatrix a b = do
    if (mSize a) == (mSize b) 
    then Just $ makeSeqMtxFromLists $ mmult (splitIntoListsByRow (mDat a) (mSize a) (mSize a)) (splitIntoListsByRow (mDat b) (mSize b) (mSize b))
    else Nothing

splitIntoListsByRow :: Seq Integer -> Int -> Int -> [[Integer]]
splitIntoListsByRow s n 1 = [toList $ (Seq.take n s)]
splitIntoListsByRow s n j = [toList $ (Seq.take n s)] ++ splitIntoListsByRow (Seq.drop n s) n (j-1)

makeSeqMtxFromLists :: [[Integer]] -> Matrix
makeSeqMtxFromLists a = M {mDat = Seq.fromList [y | x <- a, y <- x], mSize = (length a), mIx = mIx m}
    where m = newMatrix (length a)

mmult :: Num a => [[a]] -> [[a]] -> [[a]] 
mmult a b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ] `using` parTraversable rseq

test_mulMatrix :: [Bool]
test_mulMatrix =
  let ma = newMatrix 3
      mb = setMatrix (2,2) 2 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 4 $ setMatrix (2,2) 3 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ mulMatrix ma mb
      , (mDat <$> mulMatrix mb mc) == Just (Seq.fromList [1, 0, 8, 6])
      , (mDat <$> mulMatrix mc mb) == Just (Seq.fromList [1, 0, 4, 6])
      ]

main = do
    print test_newMatrix
    print test_setMatrix
    print test_getMatrix
    print test_addMatrix
    print test_mulMatrix