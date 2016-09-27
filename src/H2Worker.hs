{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module H2Worker ( h2Worker
                ) where

import Server
import H2Parser
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Request
import Control.Applicative ((<$>))
import Data.Int
import Data.Bits
import Data.Default.Class
import Debug.Trace
import Control.Monad

data H2Connection = H2Connection { lastIdentifier :: Int
                                 , streams :: [H2Stream]
                                 } deriving (Show, Eq)

data H2Stream = H2Stream { identity :: Int
                         , priority :: Int
                         , dependency :: Int
                         , ended :: Bool
                         , headers :: [H2Header]
                         } deriving (Show, Eq)

instance Default H2Stream where
  def = H2Stream { identity = 0
                 , priority = 0
                 , dependency = 0
                 , ended = False
                 , headers = []
                 }

type H2Header = (String, String)

instance Default H2Connection where
  def = H2Connection { lastIdentifier = 2
                     , streams = []
                     }


h2Worker :: RequestHandler a -> a -> Worker
h2Worker handler state "h2" send recv = do
  contents <- recv 0

  let preface = BL.take 24 contents
  print "Preface"
  print preface

  -- connection preface from server: empty SETTINGS
  let sgs = def { frameType  = SETTINGS
                , flags      = 0 -- ACK flag, indicates we acknowledge client SETTINGS
                , identifier = 0
                }

  let c = def :: H2Connection

  sendFrame sgs

  let frames = parseFrames (BL.drop 24 contents)
  foldM_ processFrame c frames

  where
    processFrame connection (Right f)
      | frameType f == HEADERS =
        let i = identifier f
        in
          if any ((== i) . identity) (streams f) then
            let index = findIndex ((== i) . identity) (streams f)
            in connection
          else
            let newstream = def { identity = i
                                , ended    = flags f .&. 1
                                }
            in connection { streams = newstream:streams connection
                       }
      | otherwise = do
        putStr "Unrecognised frame: "
        print f
        putStrLn "/Unrecognised frame"
        return connection
    processFrame c (Left err) = putStr "Error: " >> putStrLn err >> return c
    sendFrame = send . toBinary

