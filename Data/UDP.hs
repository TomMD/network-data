{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Data.UDP
	( UDPPort(..)
	, UDPHeader (..)
	) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.CSum
import Data.Header

newtype UDPPort = UDPPort Word16 deriving (Eq, Ord, Show, Read, Num, Bounded)

instance Binary UDPPort where
	put (UDPPort p) = putWord16be p
	get = getWord16be >>= return . UDPPort

data UDPHeader =
	UDPHdr  { srcPort	:: UDPPort
		, dstPort	:: UDPPort
		, payloadLength :: Int
		, checksum	:: CSum
	} deriving (Eq, Ord, Show)

instance Binary UDPHeader where
	put (UDPHdr s d l c) = do
		put s
		put d
		putWord16be $ fromIntegral l
		put c
	get = do
		s <- get
		d <- get
		l <- getWord16be >>= return . fromIntegral
		c <- get
		return $ UDPHdr s d l c
