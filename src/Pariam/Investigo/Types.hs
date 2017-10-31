{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2017 Ricky Elrod
-- License   :  BSD3
-- Maintainer:  Ricky Elrod <ricky@elrod.me>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides types used throughout Investigo.
--------------------------------------------------------------------
module Pariam.Investigo.Types where

import Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map.Lazy as L
import qualified Data.Text as T
import Servant

import Pariam.Investigo.BEncode
import Pariam.Investigo.Metadata.Types

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

-- | When a client hits the announce url to stop, start, or complete, a torrent,
-- it will include an @event@ queryparam. If it doesn't, or if it's empty, then
-- it is a normal interval-checkin. Otherwise it must be either \"started\",
-- \"stopped\", or \"completed\".
data Event = Started | Stopped | Completed deriving (Eq, Show)

instance FromHttpApiData Event where
  parseQueryParam t
    | t == T.pack "started" = Right Started
    | t == T.pack "stopped" = Right Stopped
    | t == T.pack "completed" = Right Completed
    | otherwise = Left . T.pack $ "event field is malformed"

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
                  , event      :: Maybe Event
                  , ip         :: Maybe String
                  , numwant    :: Maybe Integer
                  , key        :: Maybe String
                  , trackerid  :: Maybe String
                  } deriving (Eq, Show)

-- | Response to announce request.
data AnnounceResponse =
    Failure { failureReason :: String }
  | Response { warning :: Maybe String
             , interval :: Integer
             , minInterval :: Maybe Integer
             , arTrackerId :: Maybe String
             , complete :: Integer
             , incomplete :: Integer
             , peers :: PeerList
             }
    deriving (Eq, Show)

instance BEncodable AnnounceResponse where
  bencode (Failure s) =
    BDict (L.fromList [("failure reason", BString (C8.pack s))])
  --


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
    QueryParam "event" Event :>
    QueryParam "ip" String :>
    QueryParam "numwant" Integer :>
    QueryParam "key" String :>
    QueryParam "trackerid" String :>
    Get '[PlainText] BEncodeResponse
