{-# LANGUAGE OverloadedStrings #-}

module LearnYouNode
  where

import System.Directory (listDirectory)
import Data.List (intercalate, isSuffixOf)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Char as C
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad (join)

import Network.Wreq (get, responseBody)
import Control.Lens

import Network.Wai
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run)

-- Exercise 3
-- Read a file and print the number of newlines it contains
countLinesFile :: FilePath -> IO ()
countLinesFile fp = do
  contents <- readFile fp
  print $ (length . lines) contents

-- Exercise 5
-- Print a list of files in a given directory, filtered by the extension of the files
ls :: FilePath -> String -> IO ()
ls fp ext = do
  files <- listDirectory fp
  print $ intercalate "\n" . filter (\x -> ("." ++ ext) `isSuffixOf` x) $ files

-- Exercise 7
-- Perform a HTTP GET request to a URL provided and print the length and body
httpGet :: String -> IO ()
httpGet url = do
  r <- get url
  let body = r ^. responseBody
  print $ BS.length body
  print body

-- Exercise 9
-- Perform 3 HTTP get requests to URLs in the same order as provided
httpGet3 :: (String, String, String) -> IO ()
httpGet3 (url1, url2, url3) = do
  r <- get url1
  print $ r ^. responseBody
  r <- get url2
  print $ r ^. responseBody
  r <- get url3
  print $ r ^. responseBody

stringToUtf8 :: String -> BS.ByteString
stringToUtf8 = encodeUtf8 . T.pack

-- Exercise 10
-- Run a TCP time server
timeServer :: String -> IO ()
timeServer port = do
  putStrLn $ "http://localhost:" ++ port
  run (read port) app
  where
    app :: Application
    app _ respond = do
      t <- getZonedTime
      respond $ responseLBS
        status200
        [(hContentType, "text/plain")]
        $ stringToUtf8 (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t)

-- Exercise 11
-- Run a HTTP server that serves the same text file for each request it receives
fileServer :: String -> String -> IO ()
fileServer port file = do
  putStrLn $ "http://localhost:" ++ port
  run (read port) app
  where
    app :: Application
    app _ respond = do
      fileContents <- readFile file
      respond $ responseLBS
        status200
        [(hContentType, "text/plain")]
        $ stringToUtf8 fileContents

res404 :: Response
res404 = responseLBS
  status200
  [(hContentType, "text/plain")]
  "404 Not found"

-- Exercise 12
-- Run a HTTP server that converts incoming POST body characters to upper-case and returns it to the client
upcaseServer :: String -> IO ()
upcaseServer port = do
  putStrLn $ "http://localhost:" ++ port
  run (read port) app
  where
    app :: Application
    app request respond = do
      body <- requestBody request
      respond $ case requestMethod request of
        "POST" -> upcaseResponse (show body)
        _ -> res404
    upcaseResponse :: String -> Response
    upcaseResponse s = responseLBS
      status200
      [(hContentType, "text/plain")]
      (stringToUtf8 (map C.toUpper s))

-- Exercise 13
-- Run a HTTP server that serves JSON data when it receives a GET request
apiServer :: String -> IO ()
apiServer port = do
  putStrLn $ "http://localhost:" ++ port
  run (read port) app
  where
    app :: Application
    app request respond = do
      let query = queryString request
      let isoParam = join $ lookup "iso" query
      respond $ case pathInfo request of
        [] -> res404
        ["api", "parsetime"] -> case isoParam of
          Just x -> jsonTime $ show x
          Nothing -> res404
        ["api", "unixtime"] -> case isoParam of
          Just x -> jsonUnixTime $ show x
          Nothing -> res404
        _ -> res404
    jsonTime :: String -> Response
    jsonTime t = responseLBS
      status200
      [(hContentType, "application/json")]
      $ stringToUtf8 $ timeToJSON $ read t
    jsonUnixTime :: String -> Response
    jsonUnixTime t = responseLBS
      status200
      [(hContentType, "application/json")]
      $ stringToUtf8 $ unixTimeToJSON $ read t

timeToJSON :: String -> String
timeToJSON s =
  let m = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s :: Maybe TimeOfDay
  in
    case m of
      Nothing -> errorJSON "Invalid format"
      Just x ->
        let (TimeOfDay hour minute second) = x
        in
          "{ " ++
          "\"hour\": " ++ show hour ++ ", " ++
          "\"minute\": " ++ show minute ++ ", " ++
          "\"second\": " ++ show (floor second) ++
          " }"

unixTimeToJSON :: String -> String
unixTimeToJSON s =
  let m = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" s :: Maybe UTCTime
  in
    case m of
      Nothing -> errorJSON "Invalid format"
      Just x ->
        let unixtime = utcTimeToPOSIXSeconds x
        in "{ \"unixtime\": " ++ show unixtime ++ " }"

errorJSON :: String -> String
errorJSON m = "{\"error\": \"" ++ m ++ "\"}"
