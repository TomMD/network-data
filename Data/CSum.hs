{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Data.CSum
	( csum16
	, CSum
	, zeroCSum
	) where

import qualified Data.ByteString.Lazy as B
import Data.List (foldl')
import Control.Monad (sequence)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits

newtype CSum = CSum Word16 deriving (Eq, Ord, Show, Bounded, Num)

csum16 :: B.ByteString -> CSum
csum16 b = CSum $ foldl' ( (+) . complement) 0 words
  where
  words :: [Word16]
  words = runGet (sequence $ replicate (fromIntegral $ B.length b `div` 4) getWord16be) b
{-# INLINE csum16 #-}

instance Binary CSum where
	put (CSum w) = putWord16be w
	get = getWord16be >>= return . CSum

zeroCSum = CSum 0
