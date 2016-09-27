{-# LANGUAGE ScopedTypeVariables #-}
module H2Parser ( Frame (..)
                , FrameType (..)
                , parseFrames
                , toBinary
                , toFrameType
                , fromFrameType
                ) where

import Request
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Debug.Trace
import Data.ByteString.UTF8 (fromString, toString)
import Data.Default.Class
import Data.Bits
import Control.Applicative ((<$>))

data Frame = Frame { len        :: Word32
                   , frameType  :: FrameType
                   , flags      :: Word8
                   , r          :: Word8
                   , identifier :: Word32
                   , payload    :: BL.ByteString
                   } deriving (Eq)

instance Show Frame where
  show f = (show $ identifier f) ++ " " ++
           (show $ frameType f) ++ "\n" ++
           "Length: " ++ (show $ len f) ++ ", " ++
           "Flags: " ++ (show $ flags f) ++ ", " ++
           "R: " ++ (show $ r f) ++ "\n" ++
           "Payload: " ++ (show $ payload f)
           

instance Default Frame where
  def = Frame { len        = 0
              , frameType  = SETTINGS
              , flags      = 0
              , r          = 0
              , payload    = BL.empty
              , identifier = 0
              }

data FrameType = HEADERS | PRIORITY | RSTSTREAM | SETTINGS | PUSHPROMISE | PING | GOAWAY
               | WINDOWUPDATE | CONTINUATION deriving (Show, Eq)


parseFrames :: BL.ByteString -> [Either String Frame]
parseFrames contents = 
  parseFrame contents
  where
    parseFrame bl
      | BL.null bl = []
      | otherwise = 
        let len :: Word32         = decodeBits 24 . BL.reverse $ BL.take 3 bl
            t :: FrameType        = toFrameType . decode . BL.take 1 $ BL.drop 3 bl
            flags :: Word8        = decode . BL.take 1 $ BL.drop 4 bl
            ridentifier :: Word32 = decode . BL.take 4 $ BL.drop 5 bl
            payload               = BL.take (fromIntegral len) $ BL.drop 9 bl

            r                     = ridentifier .&. (2^4)
            identifier            = ridentifier .&. (2^4 - 1)

            total                 = 3 + 1 + 1 + 4 + fromIntegral len
        in 
          (Right $ Frame { len       = len
                        , frameType  = t
                        , flags      = flags
                        , r          = fromIntegral r
                        , identifier = identifier
                        , payload    = payload
                        }):parseFrame (BL.drop total bl)

toBinary :: Frame -> BS.ByteString
toBinary f =
  let p = BL.toStrict $ 
          (encodeBits 24 $ len f) `BL.append`
          encode (fromFrameType . frameType $ f) `BL.append`
          encode (flags f) `BL.append`
          (encode $ shift (identifier f) 1 .|. fromIntegral (r f)) `BL.append`
          payload f
  in p

decodeBits n x = 
  let parts = map (BL.index x) [0..n `div` 8 - 1]
  in foldr (\a acc -> acc `shiftL` 8 .|. fromIntegral a) 0 parts
encodeBits n x =
  let parts :: [Word8] = map (fromIntegral . (shiftR x) . (*8)) [0..n `div` 8 - 1]
  in BL.pack parts

toFrameType :: Word8 -> FrameType
toFrameType 1 = HEADERS
toFrameType 2 = PRIORITY
toFrameType 3 = RSTSTREAM
toFrameType 4 = SETTINGS
toFrameType 5 = PUSHPROMISE
toFrameType 6 = PING
toFrameType 7 = GOAWAY
toFrameType 8 = WINDOWUPDATE
toFrameType 9 = CONTINUATION

fromFrameType :: FrameType -> Word8
fromFrameType HEADERS = 1
fromFrameType PRIORITY = 2
fromFrameType RSTSTREAM = 3
fromFrameType SETTINGS = 4
fromFrameType PUSHPROMISE = 5
fromFrameType PING = 6
fromFrameType GOAWAY = 7
fromFrameType WINDOWUPDATE = 8
fromFrameType CONTINUATION = 9
