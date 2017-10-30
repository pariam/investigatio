{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Pariam.Investigo.Types where

import Data.BEncode
import Servant

-- | This type denotes a response that has been bencoded.
--
-- We wrap 'BEncode' so we can create Servant instances on it without the use of
-- orphans, such as 'MimeRender'.
data BEncodeResponse = BEncodeResponse BEncode deriving (Eq, Show)

instance MimeRender PlainText BEncodeResponse where
  mimeRender _ = \(BEncodeResponse x) -> bPack x

-- | Structure of an announcement request from a client.
data AnnounceRequest =
  AnnounceRequest { infoHash :: Maybe String
                  , peerId :: Maybe String
                  , port :: Maybe Integer
                  , uploaded :: Maybe Integer
                  , downloaded :: Maybe Integer
                  , left :: Maybe Integer
                  , compact :: Maybe Bool
                  , noPeerId :: Maybe Bool
                  , event :: Maybe String
                  , ip :: Maybe String
                  , numwant :: Maybe Integer
                  , key :: Maybe String
                  , trackerid :: Maybe String
                  } deriving (Eq, Show)

type API =
  "announce" :>
    QueryParam "info_hash" String :>
    QueryParam "peer_id" String :>
    QueryParam "port" Integer :>
    QueryParam "uploaded" Integer :>
    QueryParam "downloaded" Integer :>
    QueryParam "left" Integer :>
    QueryParam "compact" Bool :>
    QueryParam "no_peer_id" Bool :>
    QueryParam "event" String :>
    QueryParam "ip" String :>
    QueryParam "numwant" Integer :>
    QueryParam "key" String :>
    QueryParam "trackerid" String :>
    Get '[PlainText] BEncodeResponse
