{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Proxy ( handleRequest
             , State
             ) where

import Control.Concurrent
import Network.Socket hiding (send, recv)
import Control.Monad (unless)
import Network.Socket.ByteString
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as BS
import qualified Control.Concurrent.Thread.Group as TG
import Control.Exception
import Text.Printf
import Data.List
import Data.CaseInsensitive (mk)

import Server
import HTTPParser
import H2Parser
import Request

type State = (Maybe ThreadId, [(String, Socket)])

handleRequest :: RequestHandler State
handleRequest req cSend cRecv (Just tid, openSockets) = do
    killThread tid
    handleRequest req cSend cRecv (Nothing, openSockets)
handleRequest req cSend cRecv (Nothing, openSockets) =
  case lookup "Host" (requestHeaders req) of
    Nothing -> do
      cSend "HTTP/1.1 502 Bad Gateway\r\n\r\n"
      printf "Host not found in headers\n"
      return (True, (Nothing, openSockets))
    Just h  -> do
      case find (\(a, _) -> a == h) openSockets of
       Just (h, s) -> do
         isR <- isReadable s
         if isR then do
           tid <- forkIO $ handleSocket req cSend (sendAll s) (recv s)
           return (True, (Just tid, (h, s):filter (\(a, _) -> a /= h) openSockets))
         else do
           close s
           handleRequest req cSend cRecv $ (Nothing, filter (\(a, _) -> a /= h) openSockets)
       Nothing -> do
         let (host, port') = break (== ':') h
             port = if null port' then
                      "80"
                    else
                      tail port'
         s <- socket AF_INET Stream defaultProtocol
         addr <- try $ getAddrInfo Nothing (Just host) (Just port)
         case addr of
             Left (_ :: IOException) -> do
               cSend "HTTP/1.1 502 Bad Gateway\r\n\r\n"
               printf "Could not resolve address: %s\n" host
               return (True, (Nothing, openSockets))
             Right (a:_) -> do
               print $ addrAddress a
               connect s $ addrAddress a
               case requestMethod req of
                 "CONNECT" -> do
                   putStrLn $ "CONNECT " ++ h
                   handleConnect cSend cRecv (sendAll s) (recv s)
                   close s
                   return (False, (Nothing, openSockets))
                 _ -> do
                   putStrLn $ requestMethod req ++ " " ++ requestPath req
                   tid <- forkIO $ handleSocket req cSend (sendAll s) (recv s)
                   return (True, (Just tid, (h, s):openSockets))

-- handleRequest req@(H2Request _ _ _ _ _ _ _) cSend cRecv (Nothing, openSockets) = return (True, (Nothing, openSockets))

handleSocket :: (Request a) => a -> Send -> Send -> Recv -> IO ()
handleSocket req cSend sSend sRecv = do
    sSend $ fromString $ show (trim req) -- check output, trim req
    transfer
    where
        transfer = do
            b <- sRecv (2^11)
            unless (BS.null b) $ do
              cSend b
              transfer

handleConnect :: Send -> Recv -> Send -> Recv -> IO ()
handleConnect cSend cRecv sSend sRecv = do
  g <- TG.new
  cSend "HTTP/1.1 200 Ok\r\nConnection: Close\r\n\r\n"
  TG.forkIO g $ transfer sSend cRecv
  TG.forkIO g $ transfer cSend sRecv
  TG.waitN 2 g
  where transfer snd rcv = do
          b <- rcv (2^11)
          unless (BS.null b) $ do
            snd b
            transfer snd rcv

trim :: (Request a) => a -> a
trim req = setRequestPath (setRequestHeaders req headers) path
  where
      path = trimPath (requestPath req)
      headers = trimHeaders (requestHeaders req) ++ [("Connection", "Keep-Alive")]
      trimPath p = if "http" == mk (take 4 p) then
                      dropWhile (/= '/') $ drop 7 p
                    else
                      p
      trimHeaders = filter (shouldBe.fst)
      shouldBe = not.(`elem` ["Proxy-Connection", "Connection"])
