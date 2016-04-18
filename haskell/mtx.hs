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

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

data Matrix = M
  { mDat :: Seq Integer
  , mSize :: Int
  , mIx :: (Int, Int) -> Maybe Int
  }

newMatrix :: Int -> Matrix
newMatrix n = (M
  { mDat = (Seq.replicate n 0)
  , mSize = n
  , mIx = (\(a,b) -> if a>n || b>n then Nothing
                     else Just (Seq.index mDat (a+n*b))
		  )
  })
  
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
  
main = do
    print test_newMatrix