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

hGetLines :: Handle -> IO [Text]
hGetLines hdl = do
  line <- Text.hGetLine hdl
  if Text.null line
  then return []
  else do
    rest <- hGetLines hdl
    return $ line : rest

acceptHandle :: Socket -> IO Handle
acceptHandle serverSocket = do
  (hdl, _, _) <- accept serverSocket
  hSetBuffering hdl NoBuffering
  hSetNewlineMode hdl $ NewlineMode CRLF CRLF
  return hdl

response :: Text -> Text
response str = Text.unlines
  [ "HTTP/1.0 200 OK"
  , "Connection: close"
  , "Content-Type: text/plain"
  , ""
  , str
  ]

acceptFork :: Socket -> (Handle -> IO ()) -> IO ()
acceptFork socket action = do
    hdl <- acceptHandle socket
    forkIO(action hdl)
    return ()

handleClient :: Handle -> IO ()
handleClient hdl = do
    lines <- hGetLines hdl
    answer <- Text.hPutStr hdl (response "foobar")
    hFlush hdl
    hClose hdl
    return answer

requestedResource :: [Text] -> Maybe Text
requestedResource [] = Nothing
requestedResource req = do
    w <- Just $ Text.words (head req)
    if length w >= 3
    then do
        if Text.toLower (head w) == "get"
        then Text.stripPrefix "/" (w !! 1)
        else Nothing
    else Nothing

test_requestedResource =
  [ requestedResource [""]                      == Nothing
  , requestedResource ["ALMA"]                  == Nothing
  , requestedResource ["GET"]                   == Nothing
  , requestedResource ["GET /index.html"]       == Nothing
  , requestedResource ["ALMA /index.html alma"] == Nothing
  , requestedResource ["GET /index.html alma"]  == Just "index.html"
  , requestedResource ["get /index.html alma"]  == Just "index.html"
  , requestedResource ["GET /alma.html HTTP/1.1", "Korte: very"] == Just "alma.html"
  ]

handleRequest :: Text -> (Int, Int) -> (Int, Int)
handleRequest "/n" (i,j) = (i,j+1)
handleRequest "/s" (i,j) = (i,j-1)
handleRequest "/w" (i,j) = (i-1,j)
handleRequest "/e" (i,j) = (i+1,j)
handleRequest _ (i,j) = (i,j)

drawState :: (Int, Int) -> Text
drawState x = addLine 1 x

addLine :: Int -> (Int, Int) -> Text
addLine 11 (xI,xJ) = Text.append (Text.append (Text.append (Text.append "(" (fromString $ show xI)) ",") (fromString $ show xJ)) ")"
addLine j (xI,xJ) = do
    if j == xJ
    then Text.append (addX 1 xI) (addLine (j+1) (xI,xJ))
    else Text.append "..........\n" (addLine (j+1) (xI,xJ))

addX :: Int -> Int -> Text
addX 11 _ = "\n"
addX i xI = do
    if i == xI
    then (Text.append "X" (addX (i+1) xI))
    else (Text.append "." (addX (i+1) xI))

test_drawState :: [Bool]
test_drawState =
  [ drawState (-1, -1) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(-1,-1)"
  , drawState (1, 5) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \X.........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(1,5)"
  ]

-- main :: IO ()
-- main = do
    -- server <- listenOn (PortNumber 8000)
    -- acc <- forkIO(acceptFork server handleClient)
    -- getLine
    -- killThread acc
    -- sClose server
    -- return ()

main = do
    print test_requestedResource
    print test_drawState
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	