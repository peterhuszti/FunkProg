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

removeNegative :: [Int] -> [Int]
removeNegative = filter (>= 0)

prop_removeNegative :: [Int] -> Bool
prop_removeNegative list = all (\x -> x>=0) (removeNegative list)
    
drop2 :: [a] -> [a]
drop2 []        = []
drop2 [_]       = []
drop2 (_:_:xs)  = xs

prop_drop2 :: Eq a => [a] -> Bool
prop_drop2 list = (drop2 list) == (drop 2 list)

newtype ZippList a = ZPL ([a], [a])
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (ZippList a) where
    arbitrary = do
        firstList <- sequence [arbitrary | _ <- [-100..100]]
        secondList <- sequence [arbitrary | _ <- [-100..100]]
        return $ ZPL (firstList, secondList)

left :: ZippList a -> ZippList a
left (ZPL (x : xs, ys)) = ZPL (xs, x : ys)
left zl = zl

prop_left :: Eq a => ZippList a -> Bool
prop_left zl@(ZPL(a,b)) = (ZPL(tail a, (head a) : b)) == left zl

main = do
    -- quickCheck prop_removeNegative
    -- quickCheck (prop_drop2 :: [Int] -> Bool)
    -- sample (arbitrary :: Gen (ZippList Int))
    quickCheck (prop_left :: ZippList Int -> Bool)
    