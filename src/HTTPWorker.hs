{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module HTTPWorker ( httpWorker
                  ) where

import Server
import HTTPParser
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad (unless, when)
import Request
import Control.Applicative ((<$>))

httpWorker :: RequestHandler a -> a -> Worker
httpWorker handler state "http/1.1" send recv = do
  let newrecv n = BL.toStrict <$> recv n
  packet <- newrecv (2^11)

  unless (BS.null packet) $ do
    (cont, news) <- case parseHTTP packet of
      Left decoder -> do
        return (False, state)
      Right req -> handler req send newrecv state
    when cont $ httpWorker handler news "http/1.1" send recv
