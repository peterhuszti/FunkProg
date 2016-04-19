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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	