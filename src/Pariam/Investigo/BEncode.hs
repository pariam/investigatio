--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2017 Ricky Elrod
-- License   :  BSD3
-- Maintainer:  Ricky Elrod <ricky@elrod.me>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a typeclass for BEncoding data.
--------------------------------------------------------------------
module Pariam.Investigo.BEncode where

import Data.BEncode

-- | This seems somewhat-obviously missing from the 'bencode' package. This
-- defines an abstraction for bencoding our own data types.
class BEncodable a where
  -- | Actually do the encoding
  bencode :: a -> BEncode
