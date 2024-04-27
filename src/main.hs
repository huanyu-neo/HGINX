{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Control.Concurrent.Async
import Control.Exception
import System.IO
import System.Directory
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

data Config = Config
    { port :: Int
    , webRoot :: FilePath
    }

defaultConfig :: Config
defaultConfig = Config
    { port = 8080
    , webRoot = "/var/www/html"
    }

readConfig :: FilePath -> IO Config
readConfig configFile = do
    contents <- readFile configFile
    let configLines = lines contents
        settings = map parseConfigLine configLines
    return $ foldl applySetting defaultConfig settings

parseConfigLine :: String -> (String, String)
parseConfigLine line =
    let (key, value) = break (== '=') line
    in (trim key, trim $ drop 1 value)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

applySetting :: Config -> (String, String) -> Config
applySetting config ("port", value) = config { port = read value }
applySetting config ("webRoot", value) = config { webRoot = value }
applySetting config _ = config

handleRequest :: Handle -> FilePath -> IO ()
handleRequest handle webRoot = do
    request <- BS.hGetLine handle
    let fileName = webRoot ++ getRequestPath (BS.unpack request)
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            hPutStrLn handle "HTTP/1.1 200 OK"
            hPutStrLn handle "Content-Type: text/html"
            hPutStrLn handle ""
            content <- BS.readFile fileName
            BS.hPut handle content
        else do
            let status = "404 Not Found"
                errorPage = webRoot ++ "/404.html"
            pageExists <- doesFileExist errorPage
            if pageExists
                then do
                    hPutStrLn handle $ "HTTP/1.1 " ++ status
                    hPutStrLn handle $ "Content-Type: text/html"
                    hPutStrLn handle ""
                    content <- BS.readFile errorPage
                    BS.hPut handle content
                else do
                    hPutStrLn handle $ "HTTP/1.1 " ++ status
                    hPutStrLn handle $ "Content-Type: text/html"
                    hPutStrLn handle ""
                    hPutStrLn handle $ "<h1>" ++ status ++ "</h1>"
                    hPutStrLn handle "<p>The sun goes down, just like our server.</p>"
    hClose handle

getRequestPath :: String -> FilePath
getRequestPath request =
    let parts = words request
        path = parts !! 1
    in if path == "/"
        then "/index.html"
        else path

handleClient :: Socket -> FilePath -> IO ()
handleClient sock webRoot = do
    (client, _) <- accept sock
    asyncId <- async $ finally (handleRequest client webRoot) (sClose client)
    wait asyncId
    handleClient sock webRoot

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Starting Haskell server..."
    config <- readConfig "nginx.conf"
    putStrLn $ "Config: " ++ show config
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral $ port config) iNADDR_ANY)
    listen sock 1000
    putStrLn $ "Server listening on port " ++ show (port config) ++ "..."
    handleClient sock (webRoot config)
