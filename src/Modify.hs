{-# LANGUAGE Rank2Types #-}
module Modify ( modify
              , Criteria (..)
              ) where

import Worker
import HTTPParser
import H2Parser
import Request
import Data.List

data Criteria = Criteria { method :: String
                         , prefix :: String
                         , terminator :: String
                         , replacement :: String
                         } deriving (Show, Read)

modify :: [Criteria] -> RequestHandler a -> RequestHandler a
modify []     handler req = handler req
modify (c:cs) handler req =
    if match c req then
        handler (replace c req)
    else
        modify cs handler req
    where match c r = method c == requestMethod r && prefix c `isPrefixOf` requestPath r
          replace c rq = let head = prefix c
                             t1 = drop (length head) (requestPath rq)
                             cond = flip elem $ terminator c
                             tail = dropWhile (not.cond) t1
                             path = head ++ replacement c ++ tail
                         in setRequestPath rq path
