{-# LANGUAGE MultiParamTypeClasses #-}
module Types where

import Data.BEncode
import Servant

-- | This type denotes a response that has been bencoded.
--
-- We wrap 'BEncode' so we can create Servant instances on it without the use of
-- orphans, such as 'MimeRender'.
data BEncodeResponse = BEncodeResponse BEncode deriving (Eq, Show)

instance MimeRender PlainText BEncodeResponse where
  mimeRender _ = \(BEncodeResponse x) -> bPack x
