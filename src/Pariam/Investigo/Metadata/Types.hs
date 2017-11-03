{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2017 Ricky Elrod
-- License   :  BSD3
-- Maintainer:  Ricky Elrod <ricky@elrod.me>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module describes types related to how we obtain, store, and manage
-- metadata for a given torrent.
--
-- We only care about client data here. \"Actual\" metadata, such as what a
-- frontend would care about, is stored in another database, but can safely key
-- on our infohash.
--
-- The idea is to keep things pluggable, i.e., have the data be able to be
-- stored in any number of ways.
--
-- Also, we do not rely on anything in 'Pariam.Investigo.Types', so that we can
-- re-export this module from there.
--------------------------------------------------------------------
module Pariam.Investigo.Metadata.Types where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map.Lazy as L

import Pariam.Investigo.BEncode

-- | A raw (not URL-encoded) lazy 'C8.ByteString' containing a torrent's
-- infohash.
newtype InfoHash = InfoHash C8.ByteString deriving (Eq, Show)

-- | This represents a peer in the verbose (dictionary) representation.
data Peer = Peer { peerPeerId :: String
                 , peerIp :: String
                 , peerPort :: Integer
                 } deriving (Eq, Show)

instance BEncodable Peer where
  bencode (Peer id' ip' port') =
    BDict (L.fromList [ ("peer id", BString (C8.pack id'))
                      , ("ip", BString (C8.pack ip'))
                      , ("port", BInt port')
                      ])

-- | The list of peers can be represented compactly or verbosely.
--
-- We support both.
data PeerList =
    VerbosePeerList [Peer]
  | CompactPeerList String
  deriving (Eq, Show)

instance BEncodable PeerList where
  bencode (VerbosePeerList l) = BList (fmap bencode l)
  bencode (CompactPeerList l) = BString (C8.pack l)

data Metadata =
  Metadata { metadataInfohash :: String
           , metadataPeers :: [Peer]
           } deriving (Eq, Show)

data MetadataOpF md next where
  MetadataFetch :: InfoHash -> (Maybe md -> next) -> MetadataOpF md next
  MetadataUpdate :: (md -> md) -> next -> MetadataOpF md next

instance Functor (MetadataOpF md) where
  fmap f (MetadataFetch i g) = MetadataFetch i (f . g)
  fmap f (MetadataUpdate uf n) = MetadataUpdate uf (f n)

instance Show (MetadataOpF md next) where
  show (MetadataFetch ih _) = "MetadataFetch (" ++ show ih ++ ")"
  show (MetadataUpdate _ _) = "MetadataUpdate"

makeFree ''MetadataOpF

type MetadataOp = Free (MetadataOpF Metadata)
