module Main where

import Control.Monad.IO.Class
import Data.BEncode
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as B
import Data.IP
import Data.Maybe (fromJust) -- JUST FOR TESTING!
import qualified Data.Map.Lazy as L
import Network.Socket (hostAddressToTuple)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Read (readMaybe)

import Pariam.Investigo.Types

ipAndPortToByteString :: String -> Integer -> Maybe B.ByteString
ipAndPortToByteString ip port = do
  ipv4 <- readMaybe ip :: Maybe IPv4
  let (a,b,c,d) = hostAddressToTuple . toHostAddress $ ipv4
      ipBS = B.pack [a, b, c, d]
      portBS = B.toLazyByteString . B.word16BE . fromIntegral $ port
  return (B.append ipBS portBS)


failure :: String -> BEncodeResponse
failure s =
  BEncodeResponse (BDict (L.fromList [("failure reason", BString (C8.pack s))]))

sampleResponseSuccess :: BEncodeResponse
sampleResponseSuccess =
  BEncodeResponse (BDict (L.fromList [ ("interval", BInt 30)
                                     , ("peers", BString peers)
                                     ]))
  where
    peers = fromJust $ ipAndPortToByteString "127.0.0.1" 6916

announce
  :: Maybe String
  -> Maybe String
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Integer
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe String
  -> Maybe String
  -> Maybe Integer
  -> Maybe String
  -> Maybe String
  -> Handler BEncodeResponse
announce ih pid port up down left compact npi event ip numwant key tid = do
  let ar = AnnounceRequest ih pid port up down left compact npi event ip
           numwant key tid
  liftIO $ print ar
  return sampleResponseSuccess

server :: Server API
server =
  announce
  where

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 19888 app
