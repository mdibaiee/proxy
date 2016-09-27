{-# LANGUAGE Rank2Types #-}

module Request ( RequestHandler
               , Request (..)
               ) where
  import Server
  import Data.ByteString

  type RequestHandler a = (Request b) => b -> Send -> Recv -> a -> IO (Bool, a)

  class (Show a) => Request a where
    requestMethod :: a -> String
    requestHeaders :: a -> [(String, String)]
    setRequestHeaders :: a -> [(String, String)] -> a
    requestPath :: a -> String
    setRequestPath :: a -> String -> a
    parseRequest :: ByteString -> Either String a
