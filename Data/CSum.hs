{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Data.CSum
        ( csum16
        , CSum
        , zeroCSum
        ) where

import qualified Data.ByteString as B
import Data.List (foldl')
import Control.Monad (sequence)
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Bits
import Data.Data
import Data.Word

newtype CSum = CSum Word16 deriving (Eq, Ord, Show, Read, Bounded, Num, Data, Typeable)

csum16 :: B.ByteString -> CSum
csum16 b = CSum $ foldl' ( (+) . complement) 0 words
  where
  words :: [Word16]
  (Right words) = runGet (sequence $ replicate (fromIntegral $ B.length b `div` 4) getWord16be) b
{-# INLINE csum16 #-}

instance Serialize CSum where
        put (CSum w) = putWord16be w
        get = getWord16be >>= return . CSum

zeroCSum = CSum 0
