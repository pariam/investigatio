{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Pariam.Investigo.Types where

import Data.BEncode
import qualified Data.Text as T
import Servant
import Web.HttpApiData

-- | This type denotes a response that has been bencoded.
--
-- We wrap 'BEncode' so we can create Servant instances on it without the use of
-- orphans, such as 'MimeRender'.
data BEncodeResponse = BEncodeResponse BEncode deriving (Eq, Show)

instance MimeRender PlainText BEncodeResponse where
  mimeRender _ = \(BEncodeResponse x) -> bPack x

-- | Servant will not allow @1@ and @0@ for 'Bool' fields, it seems.
-- So we write our own type, isomorphic to 'Bool'.
data OneOrZero = One | Zero deriving (Eq, Show)

instance FromHttpApiData OneOrZero where
  parseQueryParam t
    | t == T.pack "1" = Right One
    | t == T.pack "0" = Right Zero
    | otherwise = Left . T.pack $ "field must be 1 or 0"

-- | Structure of an announcement request from a client.
data AnnounceRequest =
  AnnounceRequest { infoHash   :: Maybe String
                  , peerId     :: Maybe String
                  , port       :: Maybe Integer
                  , uploaded   :: Maybe Integer
                  , downloaded :: Maybe Integer
                  , left       :: Maybe Integer
                  , compact    :: Maybe OneOrZero
                  , noPeerId   :: Maybe OneOrZero
                  , event      :: Maybe String
                  , ip         :: Maybe String
                  , numwant    :: Maybe Integer
                  , key        :: Maybe String
                  , trackerid  :: Maybe String
                  } deriving (Eq, Show)

type API =
  "announce" :>
    QueryParam "info_hash" String :>
    QueryParam "peer_id" String :>
    QueryParam "port" Integer :>
    QueryParam "uploaded" Integer :>
    QueryParam "downloaded" Integer :>
    QueryParam "left" Integer :>
    QueryParam "compact" OneOrZero :>
    QueryParam "no_peer_id" OneOrZero :>
    QueryParam "event" String :>
    QueryParam "ip" String :>
    QueryParam "numwant" Integer :>
    QueryParam "key" String :>
    QueryParam "trackerid" String :>
    Get '[PlainText] BEncodeResponse
