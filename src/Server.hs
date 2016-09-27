{-# LANGUAGE ScopedTypeVariables #-}
module Server ( Send
              , Recv
              , Worker
              , ServerSettings (..)
              , HTTP (..)
              , HTTPS (..)
              , server
              ) where

import Prelude hiding (read)
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (forever)
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Network.Simple.TCP
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as TG

import Network.TLS
import Network.TLS.Extra.Cipher
import System.Posix.IO.ByteString
import Data.X509
import Data.Default.Class

import Debug.Trace
import System.IO (hFlush)

type Send = ByteString -> IO ()
type Recv = Int -> IO BS.ByteString
type Worker = String -> Send -> (Int -> IO BL.ByteString) -> IO ()

data HTTP = HTTP { httpPort :: String } deriving (Show)
instance Default HTTP where
  def = HTTP { httpPort = "8080" }

data HTTPS = HTTPS { httpsPort :: String
                   , cert :: String
                   , key  :: String
                   } deriving (Show)
instance Default HTTPS where
  def = HTTPS { httpsPort = "8081"
              , cert = ""
              , key  = ""
              }


data ServerSettings = ServerSettings { bindAddress :: String
                                     , http        :: Maybe HTTP
                                     , https       :: Maybe HTTPS
                                     } deriving (Show)
instance Default ServerSettings where
  def = ServerSettings { bindAddress = "0.0.0.0"
                       , http        = Nothing
                       , https       = Nothing
                       }


server :: ServerSettings -> Worker -> IO ()
server (ServerSettings bindAddr (Just http) (Just https)) worker =
  do
    g <- TG.new
    TG.forkIO g $ server (ServerSettings bindAddr (Just http) Nothing) worker
    TG.forkIO g $ server (ServerSettings bindAddr Nothing (Just https)) worker
    TG.waitN 2 g

server (ServerSettings bindAddr _ (Just (HTTPS port cert key))) worker =
  do
    chain <- credentialLoadX509 cert key

    let (cc, pkey) = either error id chain
        (CertificateChain c) = cc
        params = def { serverCACertificates = def c
                     , serverWantClientCert = False
                     , serverShared         = def { sharedCredentials = Credentials [(cc, pkey)] }
                     , serverSupported      = def { supportedCiphers = ciphersuite_all }
                     , serverDebug          = def
                     , serverHooks          = def { onALPNClientSuggest = Just selectProtocol }
                     } :: ServerParams

        selectProtocol :: [ByteString] -> IO ByteString
        selectProtocol [p] = return p
        selectProtocol (p:ps)
          | p == BC.pack "h2" || p == BC.pack "http/1.1" = return p
          | otherwise = selectProtocol ps

    print $ "[HTTPS] Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      ctx <- contextNew socket params
      handshake ctx
      protocol <- getNegotiatedProtocol ctx

      (readFd, writeFd) <- createPipe
      readHandle <- fdToHandle readFd
      writeHandle <- fdToHandle writeFd

      forkIO $ liftIO $ do
        let writeToHandle = liftIO $ do
              bs <- recvData ctx
              if BS.null bs
                then
                  return ()
                else
                  do
                    BS.hPut writeHandle bs
                    hFlush writeHandle
                    writeToHandle
        writeToHandle

      let write = sendData ctx . BL.fromStrict
          rcv   = (\_ -> BL.hGetContents readHandle)
          p     = maybe "http/1.1" BC.unpack protocol

      worker p write rcv `catch` \e -> print (e :: SomeException)

      bye ctx

    return ()
  where
    forceRecv _ 0 = return BS.empty
    forceRecv h n = do
        bs <- BS.hGet h n

        if BS.null bs then
            return BS.empty
        else if BS.length bs < n then do
            rem <- forceRecv h (n - BS.length bs)
            return $ bs `BS.append` rem
        else
            return bs


server (ServerSettings bindAddr (Just (HTTP port)) _) worker =
  do
    print $ "[HTTP] Listening on port " ++ port

    serve HostAny port $ \(socket, remoteAddr) -> do
      let write = send socket
          rcv n = BL.fromStrict . fromMaybe empty <$> Network.Simple.TCP.recv socket n

      worker "h2" write rcv `catch` \e -> print (e :: SomeException)

    return ()
