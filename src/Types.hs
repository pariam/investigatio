{-# LANGUAGE MultiParamTypeClasses #-}
module Types where

import Data.BEncode
import Servant

data BEncodeResponse = BEncodeResponse BEncode deriving (Eq, Show)

instance MimeRender PlainText BEncodeResponse where
  mimeRender _ = \(BEncodeResponse x) -> bPack x
