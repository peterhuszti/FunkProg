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

prop_drop2 :: [a] -> Bool
prop_drop2 [] = True
prop_drop2 a@[_] = 
    if length a == (length $ drop2 a) + 1
    then True
    else False
prop_drop2 a =
    if length a == (length $ drop2 a) + 2
    then True
    else False

newtype ZippList a = ZPL ([a], [a])
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (ZippList a) where
    arbitrary = do
        firstList <- arbitrary
        secondList <- arbitrary
        return $ ZPL (firstList, secondList)

left :: ZippList a -> ZippList a
left (ZPL (x : xs, ys)) = ZPL (xs, x : ys)
left zl = zl

prop_left :: ZippList a -> Bool
prop_left (ZPL ([],_)) = True
prop_left z@(ZPL (a,b)) = 
    let y@(ZPL (c,d)) = left z in
    if length c == (length a) - 1 && length d == (length b) + 1
    then True
    else False

main = do
    -- quickCheck prop_removeNegative
    quickCheck (prop_drop2 :: [Int] -> Bool)
    -- sample (arbitrary :: Gen (ZippList Int))
    -- quickCheck (prop_left :: ZippList Int -> Bool)
    