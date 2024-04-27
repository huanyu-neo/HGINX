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
    , nginxRules :: [(String, String)]
    }

defaultConfig :: Config
defaultConfig = Config
    { port = 8080
    , webRoot = "/var/www/html"
    , nginxRules = []
    }

parseConfigLine :: String -> (String, String)
parseConfigLine line =
    let (key, value) = break (== '=') line
    in (trim key, trim $ drop 1 value)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseNginxConfig :: String -> [(String, String)]
parseNginxConfig config = map parseNginxConfigLine (lines config)

parseNginxConfigLine :: String -> (String, String)
parseNginxConfigLine line =
    let (directive, value) = break (== ' ') line
    in (trim directive, trim $ dropWhile (== ' ') value)

readConfig :: FilePath -> IO Config
readConfig configFile = do
    contents <- readFile configFile
    let configLines = lines contents
        settings = map parseConfigLine configLines
        nginxConfig = parseNginxConfig contents
    return $ foldl applySetting (foldl applySetting defaultConfig nginxConfig) settings

applySetting :: Config -> (String, String) -> Config
applySetting config ("port", value) = config { port = read value }
applySetting config ("webRoot", value) = config { webRoot = value }
applySetting config _ = config

handleRequest :: Handle -> Config -> IO ()
handleRequest handle config = do
    request <- BS.hGetLine handle
    let modifiedPath = applyNginxRules (BS.unpack request) (nginxRules config)
        fileName = webRoot config ++ getRequestPath modifiedPath
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
                errorPage = webRoot config ++ "/404.html"
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

applyNginxRules :: String -> [(String, String)] -> String
applyNginxRules path rules =
    foldl' (\p (from, to) -> replace from to p) path rules

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace from to input@(x:xs)
    | from `isPrefixOf` input = to ++ replace from to (drop (length from) input)
    | otherwise = x : replace from to xs

handleClient :: Socket -> Config -> IO ()
handleClient sock config = do
    (client, _) <- accept sock
    asyncId <- async $ finally (handleRequest client config) (sClose client)
    wait asyncId
    handleClient sock config

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
    handleClient sock config