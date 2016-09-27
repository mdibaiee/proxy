{-# LANGUAGE Rank2Types #-}
module Worker ( worker
              , RequestHandler
              ) where

import Server
import HTTPParser
import H2Parser
import HTTPWorker
import H2Worker
import qualified Data.ByteString as BS
import Control.Monad (unless, when)
import Data.Maybe
import Request
import Data.Either

worker :: RequestHandler a -> a -> Worker
worker handler state "h2" send recv = h2Worker handler state "h2" send recv
worker handler state "http/1.1" send recv = httpWorker handler state "http/1.1" send recv
