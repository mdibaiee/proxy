module Main where

import System.Environment
import qualified Server as S
import qualified Network.Socket as S
import HTTPWorker
import Proxy
import ProxyAuth

data Settings = Settings { bindAddress :: String
                         , port :: S.PortNumber
                         , bufferSize :: Int
                         , authentication :: String
                         , realm :: String
                         } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings { bindAddress = "0.0.0.0"
                           , port = 8080
                           , bufferSize = 2^11
                           , authentication = ""
                           , realm = ""
                           }

main = do
    args <- getArgs
    let settings = parseArgs args defaultSettings
    let servSett = S.defaultSettings { S.bindAddress = bindAddress settings
                                     , S.port = port settings
                                     , S.bufferSize = bufferSize settings
                                     }
    let handler = if null (authentication settings) then
            handleRequest
        else
            proxyAuth (authentication settings) (realm settings) handleRequest
    S.server servSett.httpWorker handler $ (Nothing, [])

parseArgs :: [String] -> Settings -> Settings
parseArgs [] s = s
parseArgs ("-p":as) s = parseArgs ("--port":as) s
parseArgs ("-b":as) s = parseArgs ("--bindaddr":as) s
parseArgs ("-a":as) s = parseArgs ("--auth":as) s

parseArgs ("--port":as) s = case as of
    [] -> error "Please specify port number in front of --port"
    (p:as) -> parseArgs as $ s { port = fromInteger $ read p }

parseArgs ("--bindaddr":as) s = case as of
    [] -> error "Please specify bind address in front of --bindaddr"
    (b:as) -> parseArgs as $ s { bindAddress = b }

parseArgs ("--auth":as) s = case as of
    [] -> error "Please specify authentication in front of --auth"
    (a:as) -> parseArgs as $ s { authentication = a }

parseArgs ("--realm":as) s = case as of
    [] -> error "Please specify realm in front of --realm"
    (r:as) -> parseArgs as $ s { realm = r }
