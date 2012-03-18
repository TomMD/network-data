{-# LANGUAGE DisambiguateRecordFields, DeriveDataTypeable #-}

module Data.IPv6
	( IPv6 (..)
	, IPv6Header
	) where

import Control.Monad (sequence, when)
import qualified Data.ByteString as B
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Bits
import Data.Data
import Data.List (group)
import Numeric (showHex, readHex)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

gW8 = getWord8 >>= return . fromIntegral
gW16 = getWord16be >>= return . fromIntegral
gW32 = getWord32be >>= return . fromIntegral
pW8 = putWord8 . fromIntegral
pW16 = putWord16be . fromIntegral
pW32 = putWord32be . fromIntegral

data IPv6 = IPv6 B.ByteString deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize IPv6 where
	put (IPv6 b) = putByteString b
	get = getByteString 16 >>= return . IPv6

data IPv6Header =
	IPv6Hdr { version		:: Int
		, trafficClass		:: Int
		, flowLabel		:: Int
		, payloadLength		:: Int
		, nextHeader		:: IPv6Ext
		, hopLimit		:: Int
		, source		:: IPv6
		, destination		:: IPv6
	} deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize IPv6Header where
  put (IPv6Hdr ver tc fl len nh hop src dst) = do
	let verTCFlow = (ver .&. 0xF `shiftL` 28) .|. (tc .&. 0xFF `shiftL` 20) .|. (fl .&. 0xFFFFF)
	pW32 verTCFlow
	pW16 len
	put nh
	pW8 hop
	put src
	put dst

  get = do
	verTCFlow <- gW32
	let ver = (verTCFlow `shiftR` 28) .&. 0xF
	    tc  = (verTCFlow `shiftR` 20) .&. 0xFF
	    fl  = verTCFlow .&. 0xFFFFF
	len <- gW16
	nh  <- get
	hop <- gW8
	src <- get
	dst <- get
	return $ IPv6Hdr ver tc fl len nh hop src dst

data IPv6Ext = IPv6Ext Int deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Serialize IPv6Ext where
	get = liftM IPv6Ext getWord8
	put (IPv6Ext x) = putWord8 x

-- TODO: Header and Address instances

instance Pretty IPv6 where
	pPrint (IPv6 i) = cat . alternate colon . map (pHex 2) $ (B.unpack i)

-- Until 'hex' is part of the pretty printer
pHex nr n = text $ pad nr $ showHex n ""
  where
  pad n s = let l = length s in if l < n then replicate (n - l) '0' ++ s else drop (l - n) s

alternate :: a -> [a] -> [a]
alternate f xs = go xs
  where
  go (a:b:c:xs) = a : b : f : go (c:xs)
  go x = x
