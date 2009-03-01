{-# LANGUAGE DisambiguateRecordFields #-}

module Data.IPv6
	( IPv6 (..)
	) where

import Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits

gW8 = getWord8 >>= return . fromIntegral
gW16 = getWord16be >>= return . fromIntegral
gW32 = getWord32be >>= return . fromIntegral
pW8 = putWord8 . fromIntegral
pW16 = putWord16be . fromIntegral
pW32 = putWord32be . fromIntegral

data IPv6 = IPv6 B.ByteString deriving (Eq, Ord, Show)

instance Binary IPv6 where
	put (IPv6 b) = putLazyByteString b
	get = getLazyByteString 16 >>= return . IPv6

data IPv6Header =
	IPv6Hdr { version		:: Int
		, trafficClass		:: Int
		, flowLabel		:: Int
		, payloadLength		:: Int
		, nextHeader		:: IPv6Ext
		, hopLimit		:: Int
		, source		:: IPv6
		, destination		:: IPv6
	} deriving (Eq, Ord, Show)

instance Binary IPv6Header where
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

data IPv6Ext = E deriving (Eq, Ord, Show)

instance Binary IPv6Ext where
	get = return E
	put _ = return ()
