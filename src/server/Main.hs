module Main where

import Control.Monad.IO.Class
import Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromJust) -- JUST FOR TESTING!
import qualified Data.Map.Lazy as L
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Pariam.Investigo.Compact
import Pariam.Investigo.Types

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
  -> Maybe OneOrZero
  -> Maybe OneOrZero
  -> Maybe Event
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

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 19888 app
