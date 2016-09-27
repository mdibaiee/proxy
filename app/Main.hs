{-# LANGUAGE Rank2Types #-}
module Main where

import System.Environment
import qualified Server as S
import qualified Network.Socket as S
import Worker
import Proxy
import ProxyAuth
import Data.Default.Class
import Data.Maybe
import System.Exit
import Control.Monad
import HTTPParser
import H2Parser
import Server (Recv, Send)
import Request

data Settings = Settings { bindAddress    :: String
                         , authentication :: String
                         , realm          :: String
                         , https          :: Maybe S.HTTPS
                         , http           :: Maybe S.HTTP
                         } deriving (Show)
instance Default Settings where
  def = Settings { bindAddress    = "0.0.0.0"
                 , authentication = ""
                 , realm          = ""
                 , https          = Nothing
                 , http           = Nothing
                 }

main = do
    args <- getArgs
    let settings = parseArgs args def :: Settings
    let servSett = def { S.bindAddress = bindAddress settings
                       , S.http        = http settings
                       , S.https       = https settings
                       } :: S.ServerSettings

    when ((isJust . https) settings &&
         ((null . S.key . fromJust . https) settings ||
          (null . S.cert . fromJust . https) settings)) $ do
           print "You must specify --key and --cert for https to work"
           exitFailure

    when ((isNothing . http) settings && (isNothing . https) settings) $ do
      print "You must specify at least one of --http or --https parameters"
      exitFailure

    let handler = handleRequest :: HTTPRequest -> Send -> Recv -> State -> IO (Bool, State)
    if null (authentication settings)
      then
        S.server servSett . worker handleRequest $ (Nothing, [])
      else
        S.server servSett . worker (proxyAuth (authentication settings) (realm settings) handleRequest) $ (Nothing, [])

parseArgs :: [String] -> Settings -> Settings
parseArgs [] s = s
parseArgs ("-p":as) s = parseArgs ("--port":as) s
parseArgs ("-b":as) s = parseArgs ("--bindaddr":as) s
parseArgs ("-a":as) s = parseArgs ("--auth":as) s

parseArgs ("--bindaddr":as) s = case as of
    [] -> error "Please specify bind address in front of --bindaddr"
    (b:as) -> parseArgs as $ s { bindAddress = b }

parseArgs ("--auth":as) s = case as of
    [] -> error "Please specify authentication in front of --auth"
    (a:as) -> parseArgs as $ s { authentication = a }

parseArgs ("--realm":as) s = case as of
    [] -> error "Please specify realm in front of --realm"
    (r:as) -> parseArgs as $ s { realm = r }

parseArgs ("--http":as) s = case as of
    [] -> error "Please specify http port in front of --http"
    (r:as) -> parseArgs as $ s { http = Just (def { S.httpPort = r }) }

parseArgs ("--https":as) s = case as of
    [] -> error "Please specify https port in front of --https"
    (r:as) -> parseArgs as $ s { https = Just (def { S.httpsPort = r }) }

parseArgs ("--cert":as) s = case as of
    [] -> error "Please specify certificate path in front of --cert"
    (r:as) -> parseArgs as $ s { https = Just ((fromJust $ https s) { S.cert = r }) }

parseArgs ("--key":as) s = case as of
    [] -> error "Please specify key path in front of --key"
    (r:as) -> parseArgs as $ s { https = Just ((fromJust $ https s) { S.key = r }) }
